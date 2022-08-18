# ##### BEGIN GPL LICENSE BLOCK #####
#
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software Foundation,
#  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# ##### END GPL LICENSE BLOCK #####


# =============================================================================
# Lua-script scene exporter add-on for Blender.
# By Rafael Navega, 2022
# Github: https://github.com/RNavega/2DMeshAnimation
#
# Exports the scene as a .lua script to be used with the LÖVE game engine.
# Feel free to extend / modify this script to export the data you need.
# =============================================================================
#
# Changelog:
#
# 0.2.0, 0.2.1
# - Ported to "multiversion" format (a trick to make the same script run in 2.79, 2.8x, 3.x and up).
# - Added support for multiple UV channels.
# - Added support for multiple vertex colors, if any.
# - Code cleanup, refactoring.
#
# 0.1.0
# - Initial release.

# TODO:
# - Support bone hierarchies, so bones can be procedurally animated in game engines using things
#   like IK or simple user input. Right now all bones are parentless objects in world-space.


bl_info = {
    'name': 'Lua Scene Exporter',
    'author': 'Rafael Navega (2022)',
    'version': (0, 2, 1),
    'blender': (999, 0, 0),
    'location': 'File > Export',
    'description': 'Exports the scene meshes and armatures to a Lua script, for use with the Löve game engine',
    'category': 'Import-Export'
}


import math
from itertools import chain
from collections import deque, OrderedDict
from operator import attrgetter
# Function sleep() could be used if needed on heavy scenes. See the end of ExportLua._gatherData().
#from time import sleep

import bpy
import bmesh
from bpy_extras.io_utils import ExportHelper


# A global telling what Blender version we're on. This is used for multiversion support.
IS_BLENDER_280PLUS = bpy.app.version[:2] >= (2, 80)


class GenericData():
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)

    def items(self):
        return self.__dict__.items()


class LuaEncoder():
    DICT_TYPES = {dict, OrderedDict, GenericData}
    SEQUENCE_TYPES = {tuple, list, deque}
    PRIMITIVE_TYPES = {int, float, bool, str}

    # Makes a Lua key, seeing if the key can be written as is or if it needs quotes
    # like in ["key"], to support special characters.
    @staticmethod
    def _makeLuaKey(key):
        # Lua keys are like variable names. They can't start with a digit and can
        # have letters, numbers & underscores. So anything that remains after a
        # .strip() is done will be not those.
        if (type(key) == str
            and (key[0].isdigit() or key.strip('ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                                               'abcdefghijklmnopqrstuvwxyz'
                                               '1234567890_'))):
            return '["'+key+'"]'
        return str(key)


    @staticmethod
    def _makeIndent(level):
        return ' ' * (4 * level)


    # Recursively stringifies a data structure.
    @classmethod
    def _encodeElement(cls, element, indentLevel=0):
        elementType = type(element)
        thisIndent = cls._makeIndent(indentLevel)
        nextIndent = cls._makeIndent(indentLevel + 1)
        if element is None:
            yield 'nil'
        elif elementType in cls.DICT_TYPES:
            yield '{\n'
            template = nextIndent + '%s = %s'
            yield ',\n'.join(
                template % (cls._makeLuaKey(key), ''.join(cls._encodeElement(subElement, indentLevel+1)))
                for key, subElement in element.items()
            )
            yield '\n' + thisIndent + '}'
        elif elementType in cls.SEQUENCE_TYPES:
            yield '{'
            if len(element):
                # A sequence type will always yield something like "{content}", the difference is
                # if the items inside are other than primitive types, in which case we add some
                # indentation and newlines to make the script easier to read.
                itemType = type(element[0])
                addIndent = (itemType not in cls.PRIMITIVE_TYPES)
                if addIndent:
                    yield '\n' + nextIndent
                    itemSep = ',\n' + nextIndent
                else:
                    itemSep = ', '
                yield itemSep.join(''.join(cls._encodeElement(subElement, indentLevel+1)) for subElement in element)
                if addIndent:
                    yield '\n' + thisIndent
            yield '}'
        elif elementType == str:
            yield '"' + element + '"'
        elif elementType == bool:
            # Lowercase for Lua.
            yield 'true' if element else 'false'
        elif elementType == float:
            # Avoid negative zeroes (-0.0).
            # https://stackoverflow.com/a/11010791
            yield str(element+0.0)
        else:
            yield str(element)


    # Main function.
    # The root element should be an OrderedDict so the script will have a predictable
    # order to its keys. If it's a dict then the keys will have their own internal order.
    @classmethod
    def makeScript(cls, header, rootElement):
        return header + 'return ' + ''.join(cls._encodeElement(rootElement))


    # For debugging.
    @classmethod
    def test(cls):
        data = OrderedDict((
            ('name', 'mesh'),
            ('verts', tuple(range(20))),
            ('weights', {
                'asdf': (1, 0.135, 3, 30.300381, 61, 0.98712, 2, 17.362),
                'he': None,
                'bla': (666, 1.2035, 8, 9.02369244),
            }),
            ('bones', (
                {'name': 'blar', 'hasBone': 1, 'weights': (1, 2, 3)},
                {'name': 'goooh', 'hasBone': 0, 'weights': (44, 55, 66, 77, 88)},
            )),
            ('uvs', {
                'uvmap01': [0.351, 0.664, 0.1246],
                'uvmap02': [0.235],
                '-2dx': [0.333, 0.666, (1, 2, 3, 4), 1.3520976]
            })
        ))
        print(cls.makeLuaScript(data))


class PoseBone():
    def __init__(self, poseBone, armatureObject, exporter):
        self.armatureObject = armatureObject
        self.poseBone = poseBone
        self._relativeCo = exporter.relativeCo
        # Capture the bind transform of the bone.
        self.baseTransform = self.makeFrameTransform2D()
        self.frames = deque()


    def makeFrameTransform2D(self):
        FIX_CO = self._relativeCo
        # Need to query the armature matrix directly instead of a copy of it, since the armature object
        # itself might be animated.
        if IS_BLENDER_280PLUS:
            boneWorldMatrix = self.armatureObject.matrix_world @ self.poseBone.matrix
            # Prefer not to use the bone's matrix.col[1] vector because of some weird bone axis
            # orientation that Blender uses. It's easier to just do a (bone.tail - bone.head) as
            # an alternative way to get its pose-space aim vector.
            aimVector = FIX_CO(self.armatureObject.matrix_world @ (self.poseBone.tail - self.poseBone.head))
        else:
            boneWorldMatrix = self.armatureObject.matrix_world * self.poseBone.matrix
            aimVector = FIX_CO(self.armatureObject.matrix_world * (self.poseBone.tail - self.poseBone.head))
        
        # The bone aim angle is made always positive so it can be interpolated between frames, if
        # necessary. If negative and positive angles were interpolated they'd cause bones to flip.
        # The solution is from here: https://stackoverflow.com/a/1311124
        aimAngle = math.atan2(aimVector[1], aimVector[0])
        if aimAngle < 0.0:
            aimAngle += 6.283185307179586 # (2.0 * math.pi)
        
        # Return a flat tuple with all vectors unpacked.
        return (
            # Bone 2D position vector.
            *FIX_CO(boneWorldMatrix.translation.xy),
            # Bone 2D "aim" angle (the aim axis of the bone), on the XY plane.
            aimAngle,
            # Bone 2D scale vector.
            *boneWorldMatrix.to_scale().xy
        )


    def sampleAnimationFrame(self):
        self.frames.append(self.makeFrameTransform2D())


    def output(self):
        # The bone names are present as keys in the parent dictionary, see ArmatureExportable.output().
        framesOutput = tuple(chain.from_iterable(self.frames))
        return OrderedDict((
            ('baseTransform', self.baseTransform),
            ('frames', framesOutput or None),
        ))


class Triangle():
    def __init__(self, v1, v2, v3):
        self.vertexIndices = [v1.index, v2.index, v3.index]
        # Get the minimum local Z of these vertices, to be used as a key to sort triangles.
        self.minimumLocalZ = min(v1.co.z, v2.co.z, v3.co.z)

    def replaceVertex(self, oldIndex, newIndex):
        # Used with vertex splitting. See the splitting algorithm in MeshExportable.__init__().
        i = self.vertexIndices.index(oldIndex)
        if i != -1:
            self.vertexIndices[i] = newIndex
        else:
            raise Exception('VertexExportable %i not found in triangle' % oldIndex)

    # Syntatic sugar for when generating the triangle output in MeshExportable.output().
    def __iter__(self):
        return self.vertexIndices.__iter__()


class ArmatureExportable():
    def __init__(self, armatureObject, exportablesDict, exporter):
        self.name = armatureObject.name

        # Initialize the bone exportables from this armature, capturing their transform
        # in the **rest** (AKA bind) pose.
        oldArmatureMode = armatureObject.data.pose_position
        armatureObject.data.pose_position = 'REST'

        # Make sure the armature pose is updated to reflect this rest mode.
        exporter.updateScene()

        self.exportedBones = {
            poseBone.name: PoseBone(poseBone, armatureObject, exporter)
            for poseBone in armatureObject.pose.bones
            if poseBone.bone.use_deform or not exporter.exportDeformOnly
        }
        armatureObject.data.pose_position = oldArmatureMode

        if exporter.exportBoneAnimation:
            self.sampleAnimationFrame = self._sampleFrame
        else:
            self.sampleAnimationFrame = self._sampleNothing


    def _sampleFrame(self):
        for eb in self.exportedBones.values():
            eb.sampleAnimationFrame()


    def _sampleNothing(self):
        pass


    def output(self, exporter):
        # The armature names are present as keys in the parent dictionary, see EXPORT_SCENE_OT_lua._writeFile().
        exportedBonesDict = OrderedDict((ebName, eb.output()) for ebName, eb in self.exportedBones.items())
        return {'bones': exportedBonesDict}


class MeshExportable():
    def __init__(self, obj, exportablesDict, exporter):
        self.name = obj.name

        MESH = obj.data
        MESH_TRANSFORM = obj.matrix_world
        VERTEX_GROUPS = obj.vertex_groups

        # Support a single armature deforming this mesh (Blender actually allows unlimited
        # armatures on a mesh).
        exportedBones = { }
        if exporter.exportArmatures:
            armatureObj = self.findArmatureObject(obj)
            if armatureObj and (exporter.isVisible(armatureObj) or not exporter.ignoreHidden):
                # Create the armature exportable if it hasn't been created already (it happens
                # when the mesh was processed first from the bpy.data.objects list).
                if armatureObj.name in exportablesDict['ARMATURE']:
                    ae = exportablesDict['ARMATURE'][armatureObj.name]
                else:
                    ae = ArmatureExportable(armatureObj, exporter)
                    exportablesDict['ARMATURE'][armatureObj.name] = ae
                exportedBones = ae.exportedBones
            else:
                armatureObj = None
        else:
            armatureObj = None

        bm = bmesh.new()
        bm.from_mesh(MESH)
        bm.verts.ensure_lookup_table()
        bm.faces.ensure_lookup_table()
        TOTAL_VERTICES = len(bm.verts)

        # Make sure all faces are triangulated. This only changes the BMesh copy in memory which
        # is discarded later. This doesn't change the original mesh in the scene.
        bmesh.ops.triangulate(bm, faces=bm.faces)

        # Not sure if it's necessary to refresh the faces list again after the .triangulate() call
        # since new faces were created. Better to be safe, and it's a cheap call anyway as seen
        # in the Blender source code (BM_mesh_elem_table_ensure).
        bm.faces.ensure_lookup_table()

        # =======================================
        # Triangles.
        # List of TriangleExportable objects.
        # =======================================
        allTriangles = [Triangle(*face.verts) for face in bm.faces]

        # =======================================
        # Shape keys pre-process.
        # =======================================
        if exporter.exportShapekeys and len(bm.verts.layers.shape):
            allShapekeys = tuple(
                GenericData(name=shapeName, layer=shapeLayer, offsets=[None] * TOTAL_VERTICES,
                            shapeObject=MESH.shape_keys.key_blocks[shapeName],
                            frames=deque() if exporter.exportShapekeyAnimation else None)
                # The slice [1:] is to skip the first shape key (the basis, a redundant copy of the mesh).
                for shapeName, shapeLayer in bm.verts.layers.shape.items()[1:]
            )
            # Make a dict mapping the shape key name to the vertex group index that is masking it, for
            # all shape keys that have a vertex group masking them. This vertex group blends between
            # this shape key and the basis shape key and affects visual results.
            maskedShapekeys = {
                shape.name: VERTEX_GROUPS[shape.vertex_group].index
                for shape in MESH.shape_keys.key_blocks
                if shape.vertex_group
            }
        else:
            allShapekeys = ( )

        # =======================================
        # Vertex groups / weights pre-process.
        # =======================================
        if exporter.exportWeights:
            vertexWeights = tuple(
                GenericData(name=group.name, weights={}, isBone=(group.name in exportedBones))
                for group in VERTEX_GROUPS
            )
        else:
            vertexWeights = None

        # =======================================
        # Vertex data.
        # =======================================
        DEFORM_LAYER = (bm.verts.layers.deform and bm.verts.layers.deform[0]) or None
        # Get copies of these layer collections as iterating on them directly causes an error in 2.79.
        NAMED_UV_LAYERS = bm.loops.layers.uv.items()
        NAMED_COLOR_LAYERS = bm.loops.layers.color.items()

        vertexPositions       = [None] * TOTAL_VERTICES
        vertexUVs             = {uvName: ([None] * TOTAL_VERTICES)
                                 for uvName in bm.loops.layers.uv.keys()}
        vertexColors          = {colorName: ([None] * TOTAL_VERTICES)
                                 for colorName in bm.loops.layers.color.keys()}

        # Reused per vertex, a list that stores the offsets of the vertex on each shape key of the
        # mesh, in this format: (offset1, offset2, offset3, ...) as aligned to 'allShapekeys'.
        tempShapekeyOffsets = [None] * len(allShapekeys)

        # Per-linked-loop (face corner) attributes for a vertex.
        totalLoopAttributes = len(vertexUVs) + len(vertexColors)
        baseLoopAttributes = [None] * totalLoopAttributes
        otherLoopAttributes = [None] * totalLoopAttributes

        # Dict mapping "attribute signatures" to the current splits for the vertex.
        # Used to match linked-loops that have the same vertex attributes so they can use
        # the same split vertex.
        currentSplitIndices = { }
        # Vertex index to use on a new split, so that triangles can reference the new vertex clone.
        # This is incremented when a new vertex clone is created.
        newVertexIndex = len(bm.verts)
        allVertexSplits = deque()

        # Precompute this boolean flag to go into the weights per vertex.
        USE_WEIGHTS = (exporter.exportWeights and DEFORM_LAYER is not None)
        FIX_CO = exporter.relativeCo
        FIX_UV = self.topLeftUV

        # Used in case the vertex is split, to simplify copying weights from the current vertex
        # to all its split duplicates.
        _currentWeightData = { }

        for vertex in bm.verts:
            vertexIndex = vertex.index
            # Position.
            if IS_BLENDER_280PLUS:
                worldCo = MESH_TRANSFORM @ vertex.co
            else:
                worldCo = MESH_TRANSFORM * vertex.co
            vertexPositions[vertexIndex] = FIX_CO(worldCo)

            # Vertex group weights, if any.
            # The weights collection is a dict that maps vertex index with the weight for that vertex on that
            # group. If a vertex index is not in this dict then the group doesn't use that vertex.
            _currentWeightData.clear()
            if USE_WEIGHTS and vertex[DEFORM_LAYER]:
                for groupIndex, weight in vertex[DEFORM_LAYER].items():
                    if weight > 0.0 or not exporter.ignoreZeroWeights:
                        vertexWeights[groupIndex].weights[vertexIndex] = weight
                        _currentWeightData[groupIndex] = weight

            # Shape key offsets.
            # Make a list of (offset-list, offset) pairs for this vertex to make it easier to copy
            # these offsets in case this vertex is ever split.
            for shapeIndex, shapeData in enumerate(allShapekeys):
                # The local absolute coordinate of the vertex under this shape key.
                shapePosition = vertex[shapeData.layer]
                # Transform it to world space.
                if IS_BLENDER_280PLUS:
                    shapePosition = MESH_TRANSFORM @ shapePosition
                else:
                    shapePosition = MESH_TRANSFORM * shapePosition
                # Store the offset that this shape key holds for this vertex. That is, the
                # displacement from its base mesh position. This makes it easier to animate in a game
                # engine by adding the vertex position and the offset scaled by the shape key strength.
                # A formula like: finalPosition = vertexPosition + (shapeKeyOffset * shapeKeyStrength)
                offset = FIX_CO(shapePosition - worldCo)
                # If the vertex has any weights, and this shape key is masked, and this vertex has
                # weights for the vertex group that masks this shape key, then scale the vertex offset
                # with that weight so the result looks visually the same as in Blender.
                if (DEFORM_LAYER
                    and (shapeData.name in maskedShapekeys)
                    and maskedShapekeys[shapeData.name] in vertex[DEFORM_LAYER]
                ):
                    offset *= vertex[DEFORM_LAYER][maskedShapekeys[shapeData.name]]
                tempShapekeyOffsets[shapeIndex] = offset
                shapeData.offsets[vertexIndex] = offset

            # Collect vertex attributes that can differ per linked-loop (face corner) with this vertex.

            # Use the first linked loop of the vertex as the base source of attributes.
            # Other linked loops will serve as the source for split duplicates of this vertex.
            baseLoop = vertex.link_loops[0]
            attributeIndex = 0

            # UVs.
            # (U, V), in floats.
            for attributeIndex, uvItem in enumerate(NAMED_UV_LAYERS):
                uvName, uvLayer = uvItem
                uv = baseLoop[uvLayer].uv[:]
                baseLoopAttributes[attributeIndex] = uv
                vertexUVs[uvName][vertexIndex] = FIX_UV(uv)

            # Colors.
            # (R, G, B, A), in floats.
            for attributeIndex, colorItem in enumerate(NAMED_COLOR_LAYERS, attributeIndex + 1):
                colorName, colorLayer = colorItem
                color = baseLoop[colorLayer][:]
                baseLoopAttributes[attributeIndex] = color
                vertexColors[colorName][vertexIndex] = color

            # More can be added, like a 2nd UV set, material index of 'loop.face.material_index' etc.
            # (. . .)

            # Compare the base loop attributes with those of the other loops (face corners) that
            # this vertex belongs to, and see if it's necessary to create any vertex splits.
            # To help with that, make the "attribute signature" of the base loop, used like a hash
            # to compare the attributes from different face corners of this vertex to see if any
            # loops can be represented by the same vertex split.
            baseLoopSig = tuple(chain.from_iterable(baseLoopAttributes))

            # Note the .link_loops[1:] slice below, we skip the first loop as it's already used for
            # getting the base vertex attributes.
            currentSplitIndices.clear()
            for otherLoop in vertex.link_loops[1:]:
                # Step 1: gather all splittable vertex attributes for this loop and make a key from them.
                # UVs.
                for attributeIndex, uvItem in enumerate(NAMED_UV_LAYERS):
                    otherLoopAttributes[attributeIndex] = otherLoop[uvItem[1]].uv[:]

                # Colors.
                for attributeIndex, colorItem in enumerate(NAMED_COLOR_LAYERS, attributeIndex + 1):
                    otherLoopAttributes[attributeIndex] = otherLoop[colorItem[1]][:]

                otherLoopSig = tuple(chain.from_iterable(otherLoopAttributes))

                # Step 2: see if any of those attributes differ from the base vertex attributes, a sign
                # that a vertex split is needed.
                if otherLoopSig != baseLoopSig:
                    # Step 3: see if all attributes match with the same attributes of some preexisting
                    # split so we can reuse it and not have to create a new vertex.
                    if otherLoopSig in currentSplitIndices:
                        # Reusing a split that has the exact same attributes.
                        splitVertexIndex = currentSplitIndices[otherLoopSig]
                    else:
                        # Creating a new split vertex using this different BMLoop.
                        # Store some values to be appended later, as they need to come after the
                        # END of the original vertex data list as each index is a unique vertex.
                        allVertexSplits.append(
                            (
                                vertexPositions[vertexIndex],
                                tempShapekeyOffsets.copy(),
                                otherLoop
                            )
                        )
                        # Copy the weights of the original vertex, if any. This can be done right
                        # now since the weights use dictionaries, no specific order.
                        for groupIndex, weight in _currentWeightData.items():
                            vertexWeights[groupIndex].weights[newVertexIndex] = weight
                        # Store this split in a temporary dict so it can be reused, if necessary.
                        currentSplitIndices[otherLoopSig] = newVertexIndex
                        splitVertexIndex = newVertexIndex
                        newVertexIndex += 1
                    # Step 4: replace the vertex index with the split index, in the triangle that
                    # the other loop represents.
                    allTriangles[otherLoop.face.index].replaceVertex(vertexIndex, splitVertexIndex)

        # Append to the data lists all the vertex splits that were created, if any.
        for samePosition, sameShapekeyOffsets, otherLoop in allVertexSplits:
            vertexPositions.append(samePosition)
            for shapeIndex, shapeData in enumerate(allShapekeys):
                shapeData.offsets.append(sameShapekeyOffsets[shapeIndex])
            # UVs, read from the specific linked loop instead of the base loop of the vertex.
            for uvName, uvLayer in NAMED_UV_LAYERS:
                vertexUVs[uvName].append(FIX_UV(otherLoop[uvLayer].uv[:]))
            # Colors, same as above.
            for colorName, colorLayer in NAMED_COLOR_LAYERS:
                vertexColors[colorName].append(otherLoop[colorLayer][:])

        # Sort the mesh triangles based on the local Z of their vertices.
        # This lets the user control the visual ordering of faces of the mesh by moving them above others.
        allTriangles.sort(key=attrgetter('minimumLocalZ'))

        # See if there's an image texture to include in the exported file.
        texturePath = self.findTexturePath(obj)
        if texturePath:
            # Strip the relative path prefix, if present.
            if texturePath.startswith('//'):
                texturePath = texturePath.replace('//', '', 1)
            texturePath = texturePath.replace('\\', '/')

        # Keep a reference to important things to be used later when writing the file.

        self.vertexPositions = vertexPositions
        self.vertexUVs       = vertexUVs
        self.vertexColors    = vertexColors
        self.vertexWeights   = vertexWeights
        self.allShapekeys = allShapekeys
        self.allTriangles = allTriangles
        self.texturePath = texturePath
        self.armatureName = (armatureObj and armatureObj.name) or None
        if allShapekeys and exporter.exportShapekeyAnimation:
            self.sampleAnimationFrame = self._sampleFrame
        else:
            self.sampleAnimationFrame = self._sampleNothing


    # Blender does like OpenGL in using the bottom-left corner of the UV space as origin, so
    # we just mirror the V axis to fix this because game engines use top-left bitmap coordinates.
    @staticmethod
    def topLeftUV(uv):
        return (uv[0], 1.0-uv[1])


    @staticmethod
    def findTexturePath(obj):
        # Look for the first valid image texture on the first valid material of the mesh, then return
        # its file path.
        # We go through each material slot in the object, then branch depending on if the material is
        # using shader nodes (2.79 and up) or the old Blender Render texture slots (2.79 only).
        for materialSlot in obj.material_slots:
            if materialSlot.material:
                if materialSlot.material.use_nodes:
                    # It's using nodes.
                    # Find the active Material Output node, if any. It must have a node connected.
                    lastNode = next(
                        (
                            node.inputs['Surface'].links[0].from_node
                            for node in materialSlot.material.node_tree.nodes
                            if (node.type == 'OUTPUT_MATERIAL'
                                and node.is_active_output
                                and node.inputs['Surface']
                                and node.inputs['Surface'].links)
                        ),
                        None
                    )
                    if lastNode:
                        if lastNode.type == 'TEX_IMAGE':
                            # A Texture Image node directly connected to the material output.
                            # Try to return the image path or None if it's a blank string.
                            if lastNode.image:
                                return lastNode.image.filepath or None
                        else:
                            # Support one level of indirection: see if there's a Texture Image node
                            # connected to this last node (if it has a "Color" or "Base Color" input).
                            colorInput = (lastNode.inputs.get('Color', None)
                                          or lastNode.inputs.get('Base Color', None))
                            if colorInput and colorInput.links:
                                secondaryNode = colorInput.links[0].from_node
                                if secondaryNode.type == 'TEX_IMAGE' and secondaryNode.image:
                                    return secondaryNode.image.filepath or None
                elif not IS_BLENDER_280PLUS:
                    # On 2.79 and without shader nodes, try to find a Blender Render texture slot.
                    return next(
                        (
                            texSlot.texture.image.filepath or None
                            for texSlot in materialSlot.material.texture_slots
                            if (texSlot
                                and texSlot.texture
                                and texSlot.texture.type == 'IMAGE'
                                and texSlot.texture.image
                                and texSlot.texture.image.source != 'GENERATED')
                        ),
                        None
                    )
        return None


    @staticmethod
    def findArmatureObject(obj):
        return next(
            (
                mod.object
                for mod in obj.modifiers
                if mod.show_viewport and mod.type == 'ARMATURE'
            ),
            None
        )


    # MeshExportable.sampleAnimationFrame is set at the end of MeshExportable.__init__() as
    # either of the two following functions.
    def _sampleFrame(self):
        for shapeData in self.allShapekeys:
            shapeData.frames.append(shapeData.shapeObject.value)


    def _sampleNothing(self):
        pass


    def output(self, exporter):
        # Transform the mesh data into more primitive types, like flat lists.

        # 2D positions in a flat list: (X1, Y1, X2, Y2, ...).
        vertexPositionsOutput = tuple(chain.from_iterable(self.vertexPositions))

        # Dictionary of flat UV coordinate lists like (U1, V1, U2, V2, ...).
        vertexUVOutput = {
            uvName: tuple(chain.from_iterable(uvList))
            for uvName, uvList in self.vertexUVs.items()
        }

        # Dictionary of flat RGBA float lists like (R1, G1, B1, A1, R2, G2, B2, A2, ...).
        vertexColorOutput = {
            colorName: tuple(chain.from_iterable(colorList))
            for colorName, colorList in self.vertexColors.items()
        }

        triangleIndicesOutput = tuple(chain.from_iterable(self.allTriangles))

        # By this point self.allShapekeys will either have content or be empty, depending on the
        # exporter settings.
        shapekeysOutput = {
            shapeData.name: {'offsets': tuple(chain.from_iterable(shapeData.offsets)),
                             'frames': shapeData.frames or None}
            for shapeData in self.allShapekeys
        }

        if exporter.exportWeights == 'NONE':
            vertexGroupsOutput = None
        else:
            acceptAllGroups = (exporter.exportWeights != 'BONES')
            vertexGroupsOutput = {
                weightData.name: {'weights': tuple(chain.from_iterable(weightData.weights.items())),
                                  'isBone': weightData.isBone}
                for weightData in self.vertexWeights
                if acceptAllGroups or weightData.isBone
            }


        return OrderedDict((
            # The mesh names are present as keys in the parent dictionary, see EXPORT_SCENE_OT_lua._writeFile().
            ('totalVertices', len(self.vertexPositions)),
            ('attributes', {
                'positions': vertexPositionsOutput or None,
                'uvs': vertexUVOutput or None,
                'colors': vertexColorOutput or None
            }),
            ('vertexGroups', vertexGroupsOutput or None),
            ('shapeKeys', shapekeysOutput or None),
            ('triangles', triangleIndicesOutput or None),
            ('texture', self.texturePath or None),
            ('armatureName', self.armatureName)
        ))


class EXPORT_SCENE_OT_lua(bpy.types.Operator, ExportHelper):
    """Exports the *visible* meshes and armatures to a Lua script, for use with Love2D"""
    bl_idname = 'export_scene.lua'
    bl_label = 'Export LUA'
    bl_options = {'PRESET'}

    filename_ext = ".lua"
    filter_glob = bpy.props.StringProperty(default='*.lua', options={'HIDDEN'})

    screenOrigin = bpy.props.EnumProperty(
        name = 'Top Ortho Origin',
        description = 'Where the scene origin is located in the Top Ortho view, in relation to your ' \
                      'scene contents (if in doubt, use Top-Left)',
        items = (
            ('TOPLEFT', 'Top-Left', 'The scene origin is either at the top-left corner of the scene contents ' \
                                    'or centered on them'),
            ('BOTTOMLEFT', 'Bottom-Left', 'The scene origin is at the bottom-left corner of the scene contents')
        ),
        default = 'TOPLEFT'
    )
    exportMeshes = bpy.props.BoolProperty(
        name = 'Export Meshes',
        description = (
            'Exports meshes, with vertex positions, UVs etc. (default: on)'
            'A reason to turn this off is if you only wanted bones to be exported'
        ),
        default = True
    )
    exportWeights = bpy.props.EnumProperty(
        name = 'Vertex Weights',
        items = (
            ('NONE', 'None', 'Don\'t export vertex weights.'),
            ('BONES', 'Only Bone Weights', 'Only export weights from the vertex groups used by armature bones.'),
            ('ALL', 'All Weights', 'Export all weights, from all vertex groups.')
        ),
        default = 'BONES'
    )
    ignoreZeroWeights = bpy.props.BoolProperty(
        name = 'Ignore Zero Weights',
        description = 'Ignore any vertex weights that are zero, only export non-zero weights (default: on)',
        default = True
    )
    exportShapekeys = bpy.props.BoolProperty(
        name = 'Shape Keys',
        description = 'Export mesh shape keys (AKA morph targets), if the mesh has them (default: on)',
        default = True
    )
    exportArmatures = bpy.props.BoolProperty(
        name = 'Export Armatures',
        description = (
            'Export bones from the armatures in the scene, if present (default: on)'
        ),
        default = True
    )
    exportDeformOnly = bpy.props.BoolProperty(
        name = 'Only Deform Bones',
        description = (
            'Only export bones that have their Deform setting turned on. Keep this on if your armatures '
            'contain non-Deform "helper" bones that should not be exported (default: on)'
        ),
        default = True
    )
    exportAnimation = bpy.props.BoolProperty(
        name = 'Export Animation',
        description = (
            'Export baked animation frames (default: on)'
        ),
        default = False
    )
    exportBoneAnimation = bpy.props.BoolProperty(
        name = 'Bone Animation',
        description = 'Bake frames for all exported bones (default: on)',
        default = True
    )
    exportShapekeyAnimation = bpy.props.BoolProperty(
        name = 'Shape Key Animation',
        description = 'Bake frames for the strength of exported shape keys (default: on)',
        default = True
    )
    ignoreHidden = bpy.props.BoolProperty(
        name = 'Ignore Hidden Objects',
        description = (
            'Do not export any objects that are hidden. Armatures are an exception in that if an invisible ' \
            'armature is being used by a visible mesh, that armature will be exported as well (default: on)'
        ),
        default = True
    )


    @staticmethod
    def isVisible(obj):
        if IS_BLENDER_280PLUS:
            return not (obj.hide_viewport or obj.hide_get())
        else:
            return obj.is_visible(bpy.context.scene)

    # The Blender scene XY plane as seen from the TOP ORTHO view has the Y axis pointing up.
    # If the user staged their 2D scene with the scene origin at...
    #
    # ...The TOP-LEFT corner of the viewport, then all coordinates need to be flipped on their Y
    #    axis so they look correct on a top-left coord. system used by game engines.
    #
    # ...The BOTTOM-LEFT corner of the viewport, then all coordinates need to have their Y value
    #    subtracted from the render resolution height.
    def relativeCo(self, co):
        # The value for '_screenHeight' is calculated inside EXPORT_SCENE_OT_lua._gatherData().
        return (co[0], self._screenHeight - co[1])


    @staticmethod
    def getSceneFrameRange(context):
        scene = context.scene
        if scene.use_preview_range:
            return (scene.frame_preview_start, scene.frame_preview_end)
        else:
            return (scene.frame_start, scene.frame_end)


    @staticmethod
    def updateScene():
        if IS_BLENDER_280PLUS:
            bpy.context.evaluated_depsgraph_get()
        else:
            bpy.context.scene.update()


    def _gatherData(self, context):
        if not self.exportMeshes and not self.exportArmatures:
            self.report({'INFO'}, 'All object types disabled, nothing exported')
            return None

        # Setup the screen origin coordinate conversion.
        if self.screenOrigin == 'TOPLEFT':
            self._screenHeight = 0
        else:
            self._screenHeight = context.scene.render.resolution_y

        # =============================================================================
        # Gathering data from the scene.
        # =============================================================================
        exportablesDict = { }
        if self.exportMeshes:
            exportablesDict['MESH'] = { }
        if self.exportArmatures:
            exportablesDict['ARMATURE'] = { }

        filterFunc = lambda obj: ((not self.ignoreHidden or self.isVisible(obj))
                                  and (obj.type in exportablesDict)
                                  and (obj.name not in exportablesDict[obj.type]))

        for obj in filter(filterFunc, bpy.data.objects):
            if obj.type == 'MESH':
                exportablesDict['MESH'][obj.name] = MeshExportable(obj, exportablesDict, exporter=self)
            elif obj.type == 'ARMATURE':
                exportablesDict['ARMATURE'][obj.name] = ArmatureExportable(obj, exportablesDict, exporter=self)

        # Update the scene for the last armature that temporarily changed its pose mode.
        if self.exportArmatures and exportablesDict['ARMATURE']:
            self.updateScene()

        C = context

        if not exportablesDict:
            self.report({'INFO'}, 'No visible meshes or armatures, nothing exported')
            return None

        # Get the visible start and end frame numbers.
        FRAME_START, FRAME_END = self.getSceneFrameRange(context)

        # =============================================================================
        # Sampling animation data, AKA animation baking.
        # =============================================================================
        if self.exportAnimation:
            # We sample the bone POSE-SPACE matrices as well as the shape key values at every frame of the
            # FRAME_START, FRAME_END range. There might be animation constraints or complex drivers involved in the
            # animation, the only way to export these is to sample the resulting matrix / value that they affected.
            #
            # If we kept the keyframes themselves, the problem would be that they'd need to be interpolated back
            # in the game engine in the exact same way that Blender does it (eg with Bézier curve easing). The
            # algorithm for that is in function "fcurve_eval_keyframes" of "fcurve.c" of the Blender source code.
            #
            # A possible compromise is to bake animation, but then simplify it with linear keyframes, using some
            # user-set tolerance level for quality, with 100% being the same as linear keyframes on every frame.

            # Supposedly this shows a progress bar "at the mouse cursor" (https://blender.stackexchange.com/a/3297).
            C.window_manager.progress_begin(FRAME_START, FRAME_END)
            frameToRestore = C.scene.frame_current
            for currentFrame in range(FRAME_START, FRAME_END + 1):
                # Change the scene frame. Thanks to MrRabbit (https://blender.stackexchange.com/a/133540/29992)
                C.scene.frame_set(currentFrame)
                self.updateScene()

                # Ask all top-level exportables (meshes, armatures) to sample their relevant animation.
                # For meshes this is the shape key strengths in the frame.
                # For armatures this is the transform of each pose bone in the frame.
                for exportablesCategory in exportablesDict.values():
                    for exportable in exportablesCategory.values():
                        exportable.sampleAnimationFrame()

                C.window_manager.progress_update(currentFrame)
                #sleep(0.1) # Ease the CPU on heavier scenes?

            C.window_manager.progress_end()
            C.scene.frame_set(frameToRestore)
            self.updateScene()
            # Add some information about the animation in the scene.
            # The start and end frames are inclusive.
            exportablesDict['ANIMATION'] = {'length': FRAME_END - FRAME_START + 1,
                                            'fps': float(C.scene.render.fps)}

        return exportablesDict


    def _writeFile(self, exportablesDict):
        # =============================================================================
        # Exporting to file.
        # The output is a Lua script that returns a table like this:
        #
        # -- io_scene_lua v0.1.0
        # return {
        #     (object data tables)
        # }
        # =============================================================================
        rootElement = OrderedDict()
        if exportablesDict['ANIMATION']:
            rootElement['animation'] = exportablesDict['ANIMATION']

        if exportablesDict['MESH']:
            rootElement['meshes'] = {me.name: me.output(exporter=self)
                                     for me in exportablesDict['MESH'].values()}
        if exportablesDict['ARMATURE']:
            rootElement['armatures'] = {ae.name: ae.output(exporter=self)
                                        for ae in exportablesDict['ARMATURE'].values()}

        output = LuaEncoder.makeScript(
            '-- io_scene_lua v0.1.0\n',
            rootElement
        )

        # The path we're writing to is in the 'self.filepath' attribute that comes from the
        # bpy_extras.io_utils.ExportHelper class that this ExportLua operator class inherits, and was
        # automatically set after the user confirmed the file selection dialog.
        with open(self.filepath, 'w', encoding='utf-8') as f:
            f.write(output)


    def draw(self, context):
        layout = self.layout
        scene = context.scene

        box = layout.box().column(align=True)
        box.label(text='Screen Size: %i x %i' % (scene.render.resolution_x, scene.render.resolution_y))
        box.label(text='Top Ortho Origin:')
        box.row(align=True).prop(self, 'screenOrigin', expand=True)
        #box.separator()

        layout.separator()

        block = layout.column(align=True)

        header = block.box().row(align=True)
        header.alignment = 'CENTER'
        header.label(text=' ', icon='MESH_DATA')
        header.prop(self, 'exportMeshes', toggle=True)
        col = block.box().column()
        col.enabled = self.exportMeshes
        col.prop(self, 'exportShapekeys')
        col.label(text='Vertex Weights:')
        subRow = col.row(align=True)
        subRow.alignment = 'LEFT'
        subRow.prop(self, 'exportWeights', text='')
        subRow = col.row(align=True)
        subRow.enabled = (self.exportWeights != 'NONE')
        subRow.prop(self, 'ignoreZeroWeights')

        layout.separator()

        block = layout.column(align=True)

        header = block.box().row(align=True)
        header.alignment = 'CENTER'
        header.label(text=' ', icon='ARMATURE_DATA')
        header.prop(self, 'exportArmatures', toggle=True)
        col = block.box().column()
        col.enabled = self.exportArmatures
        col.prop(self, 'exportDeformOnly')

        layout.separator()

        block = layout.column(align=True)

        header = block.box().row(align=True)
        header.alignment = 'CENTER'
        header.label(text=' ', icon='SEQUENCE')
        header.prop(self, 'exportAnimation', toggle=True)
        col = block.box().column(align=True)
        col.enabled = self.exportAnimation
        frameStart, frameEnd = self.getSceneFrameRange(context)
        col.label(text='%i frames (from %i to %i)' % (frameEnd - frameStart + 1, frameStart, frameEnd))
        col.separator()
        subRow = col.row(align=True)
        subRow.enabled = self.exportArmatures
        subRow.prop(self, 'exportBoneAnimation')
        subRow = col.row(align=True)
        subRow.enabled = self.exportMeshes and self.exportShapekeys
        subRow.prop(self, 'exportShapekeyAnimation')

        layout.separator()

        block = layout.column(align=True)

        header = block.box().row(align=True)
        header.alignment = 'CENTER'
        header.label(text='Other', icon='MODIFIER')
        col = block.box().column()
        col.prop(self, 'ignoreHidden')


    def execute(self, context):
        # Make sure to work on Object mode to avoid surprises.
        self.restoreMode = (context.mode != 'OBJECT')
        if self.restoreMode:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=True)

        exportablesDict = self._gatherData(context)

        if self.restoreMode:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=True)

        if exportablesDict:
            self._writeFile(exportablesDict)
            return {'FINISHED'}
        else:
            return {'CANCELLED'}


def menuFuncExport(self, context):
    self.layout.operator(EXPORT_SCENE_OT_lua.bl_idname, text="Lua Script (.lua)")


def multiversionClasses():
    _CLASSES = (
        EXPORT_SCENE_OT_lua,
    )

    if IS_BLENDER_280PLUS:
        # Thanks to MagicUV on Github for the compatibility recipe.
        _BASE_PROP_TYPE = getattr(bpy.props, '_PropertyDeferred', tuple)

        for cls in _CLASSES:
            if hasattr(cls, '__annotations__'):
                annotations = cls.__annotations__
            else:
                annotations = {}
                setattr(cls, '__annotations__', annotations)

            for name, value in list(cls.__dict__.items()):
                if isinstance(value, _BASE_PROP_TYPE):
                    annotations[name] = value
                    delattr(cls, name)
            yield cls
    else:
        for cls in _CLASSES:
            yield cls


def register():
    for cls in multiversionClasses():
        bpy.utils.register_class(cls)

    if IS_BLENDER_280PLUS:
        bpy.types.TOPBAR_MT_file_export.append(menuFuncExport)
    else:
        bpy.types.INFO_MT_file_export.append(menuFuncExport)


def unregister():
    if IS_BLENDER_280PLUS:
        bpy.types.TOPBAR_MT_file_export.remove(menuFuncExport)
    else:
        bpy.types.INFO_MT_file_export.remove(menuFuncExport)

    for cls in multiversionClasses():
        bpy.utils.unregister_class(cls)


if __name__ == "__main__":
    register()
