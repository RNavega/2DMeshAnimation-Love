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
# Lua-script scene exporter add-on, for Blender 2.79.
# By Rafael Navega, 2020
# Github: https://github.com/RNavega/2DMeshAnimation
#
# Exports the scene as a .lua script to be used with the LÖVE (Love2D) game
# engine. Feel free to extend / modify this script to export the data you need.
# =============================================================================
#
# Changelog:
# 0.1.0
# - Initial release.


bl_info = {
    'name': 'Love2D Lua script',
    'author': 'Rafael Navega (2020)',
    'version': (0, 1, 0),
    'blender': (2, 79, 0),
    'location': 'File > Export',
    'description': 'Exports the *visible* meshes and armatures to a Lua script, for use with Love2D',
    'category': 'Import-Export'
}


import math
from time import sleep
from itertools import chain
from collections import deque

import bpy
import bmesh
from mathutils import Matrix
from bpy_extras.io_utils import ExportHelper


# Helper class to create the output script.
class OutputWriter():
    INDENT = '\t' # Alternative, using spaces: ' ' * 4
    FIELD_NEW_LINE = ',\n'

    def __init__(self, *args, **kwargs):
        self.chunks = deque()

    def write(self, text):
        self.chunks.append(text)

    def writeIndent(self, text, indentLevel):
        self.chunks.append(self.INDENT * indentLevel + text)

    def finishField(self):
        self.chunks.append(self.FIELD_NEW_LINE)

    def finishLine(self):
        if not self.chunks[-1].endswith('\n'):
            self.chunks.append('\n')

    def full(self):
        return ''.join(self.chunks)

    @classmethod
    def formatTableVertical(cls, iterable, indentLevel, enclose=True):
        _indent = cls.INDENT * indentLevel
        return ('{\n%s%s\n%s}' if enclose else '%s%s%s') % (
            _indent,
            (cls.FIELD_NEW_LINE + _indent).join(map(str, iterable)),
            cls.INDENT * (indentLevel - 1), # One less indent level for the closing table bracket.
        )

    @staticmethod
    def formatTable(iterable):
        return '{%s}' % ', '.join(map(str, iterable))


class GenericExportable():
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)


class BoneExportable():
    def __init__(self, poseBone, armatureObject):
        self.poseBone = poseBone
        self.armatureObject = armatureObject
        self.baseTransform = self.makeFrameTransform2D()
        self.frames = deque()


    def getBoneAimAngle(self):
        # Prefer not to use the bone matrix.col[1] vector because of some weird bone axis orientation that Blender
        # uses. It's easier to just do a (bone.tail - bone.head) as an alternative way to get its pose-space aim vector.
        aimVector = ExportLUA.FLIP_Y * (self.armatureObject.matrix_world * (self.poseBone.tail - self.poseBone.head)).xy
        # Angle of the Y axis of the bone (AKA its length/aim axis, in Blender).
        return math.atan2(aimVector.y, aimVector.x)


    def makeFrameTransform2D(self):
        # Need to query the armature matrix directly instead of a copy of it, since the armature object
        # itself might be animated.
        worldMatrix = self.armatureObject.matrix_world * self.poseBone.matrix
        return GenericExportable(
            # Bone 2D position vector.
            position = (ExportLUA.FLIP_Y * worldMatrix.translation.xy),
            # Bone "aim" angle in radians, on the XY plane.
            angle = self.getBoneAimAngle(),
            # Bone 2D scale vector.
            scale = worldMatrix.to_scale().xy
        )


class ArmatureExportable():
    def __init__(self, armatureObject, exportDeformOnly):
        self.name = armatureObject.name

        # Initialize the bone exportables from this armature, capturing their transform in REST (AKA "bind") pose.
        oldArmatureMode = armatureObject.data.pose_position
        armatureObject.data.pose_position = 'REST'
        bpy.context.scene.update() # Make sure the rest pose is updated in all the pose bones.
        self.deformBones = tuple(
            BoneExportable(poseBone, armatureObject)
            for poseBone in armatureObject.pose.bones
            if not exportDeformOnly or poseBone.bone.use_deform
        )
        armatureObject.data.pose_position = oldArmatureMode

        self.deformBoneIndices = {boneData.poseBone.name: index for index, boneData in enumerate(self.deformBones)}


    def sampleAnimationFrame(self):
        for boneData in self.deformBones:
            boneData.frames.append(boneData.makeFrameTransform2D())


    def outputContents(self, output, indentOne, settings):
        indentTwo   = indentOne + 1

        # A series of bone tables, each holds data about a single bone.
        output.write(
            output.formatTableVertical(
                (
                    output.formatTableVertical(
                        (
                            'name = "%s"' % boneData.poseBone.name,
                            'baseTransform = ' + output.formatTable(
                                (
                                    boneData.baseTransform.position.x,
                                    boneData.baseTransform.position.y,
                                    boneData.baseTransform.angle,
                                    boneData.baseTransform.scale.x,
                                    boneData.baseTransform.scale.y
                                )
                            ),
                            'frames = ' + (
                                output.formatTable(
                                    # Each frame-transform is its own table for easier querying from
                                    # Lua, at the cost of more memory.
                                    output.formatTable(
                                        (
                                            frameTransform.position.x,
                                            frameTransform.position.y,
                                            frameTransform.angle,
                                            frameTransform.scale.x,
                                            frameTransform.scale.y
                                        )
                                    )
                                    for frameTransform in boneData.frames
                                ) if (settings.exportAnimation and settings.exportBoneAnimation)
                                else (
                                    'nil'
                                )
                            )
                        ),
                        indentTwo
                    )
                    for boneData in self.deformBones
                ),
                indentOne,
                enclose = False
            )
        )


class TriangleExportable():
    def __init__(self, indexIterable):
        self.vertexIndices = list(indexIterable)

    def replaceVertex(self, oldVertexIndex, newVertexIndex):
        i = self.vertexIndices.index(oldVertexIndex)
        if i != -1:
            self.vertexIndices[i] = newVertexIndex
        else:
            raise Exception('VertexExportable %i not found in triangle' % oldVertexIndex)

    def getZOrderKey(self, allVertices):
        # Used in 'MeshExportable.__init__()' to sort triangles of the same mesh based on the
        # (minimum) Z coordinate of their vertices. This lets you control the Z-order of triangles within
        # the same mesh, like separate parts of the mesh, by moving these faces along the (local) Z axis.
        # Under a Top Ortho view, this won't make any visual difference other than with ordering.
        return min(allVertices[index].worldZ for index in self.vertexIndices)

    def __getitem__(self, index):
        return self.vertexIndices[index]


class VertexExportable():
    def __init__(self, bmVertex, worldMatrix, uvList):
        worldCo = worldMatrix * bmVertex.co
        self.worldZ = worldCo.z
        # Base world-space vertex location, without any shape key deformations.
        self.outputCo = ExportLUA.FLIP_Y * worldCo.xy
        # A flat array of the UV coordinates of this vertex: (U1, V1, U2, V2...).
        self.uvs = uvList


class MeshExportable():
    def __init__(self, obj):
        self.name = obj.name

        MESH = obj.data

        bm = bmesh.new()
        bm.from_mesh(MESH)
        bm.verts.ensure_lookup_table()
        bm.faces.ensure_lookup_table()

        # Make sure all faces are triangulated.
        # Only changes the BMesh copy in memory, discarded later. Doesn't change the original mesh in the scene.
        bmesh.ops.triangulate(bm, faces=bm.faces)

        # Not sure if it's necessary to do it again after the .triangulate() call, better to be safe.
        bm.faces.ensure_lookup_table()

        # Triangle data: list of TriangleExportable objects, the order/index is the same as in the mesh.
        allTriangles = [
            TriangleExportable(vertex.index for vertex in face.verts) for face in bm.faces
        ]

        UV_LAYERS = bm.loops.layers.uv.values() # Can't iterate directly on '.layers.uv', causes an error in 2.79.

        MESH_TRANSFORM = obj.matrix_world
        VERTEX_GROUPS = obj.vertex_groups

        # The vertex group (AKA vertex weights) layer.
        DEFORM_LAYER = (bm.verts.layers.deform and bm.verts.layers.deform[0]) or None

        allShapekeys = {
            shapeName: GenericExportable(name=shapeName, layer=shapeLayer, offsets=deque(), frames=deque())
            for shapeName, shapeLayer in bm.verts.layers.shape.items()
        }
        if len(allShapekeys):
            # Remove the basis shape key, as it's a plain copy of the mesh and not needed in the output.
            allShapekeys.pop(MESH.shape_keys.key_blocks[0].name)
            # Make a dict with any shape keys that are masked by a vertex group, mapping the shape key
            # name to the vertex group index masking it.
            maskedShapeKeys = {
                shape.name: VERTEX_GROUPS[shape.vertex_group].index
                for shape in MESH.shape_keys.key_blocks
                if shape.vertex_group
            }

        allWeights = deque()
        hasAnyWeights = False

        # Go through each vertex in the mesh, seeing if it's necessary to split them when their loops (face corners)
        # have different UV coordinates.
        # A single geometry vertex might be shared by several faces. In case that vertex has different UV coordinates
        # on two or more face corners it belongs to, we need to create extra vertices to hold those unique UVs.
        allVertices = deque()
        # Temporary list to collect new vertex splits (and vertex weights and shape key positions) to be appended to
        # the data lists later, so they don't mess with the original vertex order the triangles rely on.
        allVertexSplits = deque()

        # Dict with keys as UV coordinates (or some other vertex attribute), and values as the indices of existing
        # vertex splits of a specific vertex. This is cleared on each original vertex. Used to tell if a vertex
        # split already exists for a certain attribute, so we can reuse it, or create a new one when necessary.
        attributeToSplitIndex = { }
        # Vertex index to use on a new split, so that triangles can reference the new vertex.
        newVertexIndex = len(bm.verts)

        for vertex in bm.verts:
            baseUVs = self._makeUVList(vertex.link_loops[0], UV_LAYERS)
            vertexData = VertexExportable(vertex, MESH_TRANSFORM, baseUVs)
            allVertices.append(vertexData)

            # Make a list of (vertex-group-name, weight) pairs for this vertex.
            vertexWeights = [
                (VERTEX_GROUPS[groupIndex].name, weight)
                for groupIndex, weight in (vertex[DEFORM_LAYER].items() if DEFORM_LAYER else ( ))
            ]
            hasAnyWeights = (vertexWeights or hasAnyWeights)
            allWeights.append(vertexWeights)

            # Make a list of (shape-key-exportable, offset) pairs for this vertex.
            vertexShapekeys = deque()
            for shapeName, shapeData in allShapekeys.items():
                shapePosition = vertex[shapeData.layer]
                if (
                    DEFORM_LAYER
                    and (shapeName in maskedShapeKeys and maskedShapeKeys[shapeName] in vertex[DEFORM_LAYER])
                ):
                    shapePosition = MESH_TRANSFORM * (shapePosition * vertex[DEFORM_LAYER][maskedShapeKeys[shapeName]])
                else:
                    shapePosition = MESH_TRANSFORM * shapePosition
                # Store the offset, that is, the vertex position on that shapekey, relative to the base vertex
                # position. This makes it easier to animate in the game engine by just adding this weighted offset
                # into the base vertex position: vertexPosition + (shapekeyOffset[vertexIndex] * shapekeyStrength)
                offset = (ExportLUA.FLIP_Y * shapePosition.xy) - vertexData.outputCo
                vertexShapekeys.append((shapeData.offsets, offset))
                shapeData.offsets.append(offset)

            # Compare vertex attributes with the other loops (face corners) this vertex belongs to.

            attributeToSplitIndex.clear()
            # Skip the first loop (0), since we already used it for the base vertex.
            for loop in vertex.link_loops[1:]:
                loopUVs = self._makeUVList(loop, UV_LAYERS)
                if loopUVs != baseUVs:
                    # This face corner uses a different UV than the first face corner that the vertex belongs to.
                    # See if we already have a split vertex with the same UVs as this corner, or create a new split.
                    if loopUVs in attributeToSplitIndex:
                        # Reuse a split that has the exact same UVs.
                        splitVertexIndex = attributeToSplitIndex[loopUVs]
                    else:
                        # Add a new split for this vertex, copying all except the loop UVs.
                        allVertexSplits.append(
                            (
                                VertexExportable(vertex, MESH_TRANSFORM, loopUVs),
                                vertexWeights,
                                vertexShapekeys
                            )
                        )
                        attributeToSplitIndex[loopUVs] = splitVertexIndex = newVertexIndex
                        newVertexIndex += 1

                    # Replace the vertex index in the triangle by the split index (no matter if new or reused).
                    allTriangles[loop.face.index].replaceVertex(loop.vert.index, splitVertexIndex)

            # Can also split on other attributes, like vertex colors:
            # attributeToSplitIndex.clear(); baseColors = vertex.link_loops[0][bm.loops.layers.color]
            # ... (compare with the colors on each BMLoop of vertex.link_loops[1:], as color is a loop data layer).
            # Reuse splits as possible.
            #
            # Can also split on materials:
            # attributeToSplitIndex.clear(); baseMaterialIndex = vertex.link_loops[0].face.material_index
            # ... (compare with the material-index of the BMFace of each BMLoop in vertex.link_loops[1:]).
            # Reuse splits as possible.

        # Finally, store new vertices vertex (and any splits that were done) to be exported later.
        for splitVertex, splitWeights, splitShapekeyData in allVertexSplits:
            allVertices.append(splitVertex)
            allWeights.append(splitWeights)
            for offsetList, offset in splitShapekeyData:
                offsetList.append(offset)

        # Sort 'allTriangles' based on distance towards the screen, so the Z-ordering on the viewport
        # is preserved in the output file. Triangles with lower Z should appear first so they're covered by others.
        allTriangles.sort(key=lambda triangle: triangle.getZOrderKey(allVertices))

        # See if there's a texture name to include, looking for the first valid image texture on the first
        # valid material on the mesh. We go through each material slot in the object that has a material, then
        # through each texture slot in the material and see if there's any 'IMAGE' type textures with an image path.
        texturePath = next(
            (
                texSlot.texture.image.filepath
                for matSlot in obj.material_slots if matSlot.material
                for texSlot in matSlot.material.texture_slots if (
                    texSlot.texture
                    and texSlot.texture.type == 'IMAGE'
                    and texSlot.texture.image
                    and texSlot.texture.image.filepath
                )
            ),
            None
        )
        if texturePath:
            # Keep the texture filename only.
            if texturePath.startswith('//'): # Blender relative-style mapping.
                texturePath = texturePath.replace('//', '', 1)
            else:
                texturePath = texturePath.rsplit('\\', 1)[1] # It has the full path, take the filename only.

        # Keep a reference to important things to be used later when this exportable will be asked to
        # write itself to the output.
        self.allVertices = allVertices
        self.totalUVLayers = len(UV_LAYERS)
        self.allTriangles = allTriangles
        self.allWeights = allWeights if hasAnyWeights else None
        self.allShapekeys = allShapekeys
        self.shapeKeyBlocks = MESH.shape_keys.key_blocks if allShapekeys else None
        self.texturePath = texturePath
        self.armature = None


    def _makeUVList(self, vertexLoop, uvLayers):
        # Returns a flat tuple with the components of 2D vectors from UV coordinates: (U1, V1, U2, V2 ...).
        return tuple(
            chain.from_iterable(ExportLUA.V_COMPLEMENT(vertexLoop[layer].uv.xy) for layer in uvLayers)
        )


    @staticmethod
    def findArmatureObject(meshObject):
        return next(
            (
                mod.object
                for mod in meshObject.modifiers
                if mod.show_viewport and mod.type == 'ARMATURE' and mod.object
            ),
            None
        )


    def setArmatureExportable(self, ae):
        # Used in MeshExportable.outputContents(), when exporting weights and armature name.
        self.armature = ae


    def sampleAnimationFrame(self):
        for shapeName, shapeData in self.allShapekeys.items():
            shapeData.frames.append(self.shapeKeyBlocks[shapeName].value)


    def outputContents(self, output, indentOne, settings):
        indentTwo   = indentOne + 1
        indentThree = indentOne + 2

        # Add the contents of this exportable to the OutputWriter instance sent in.
        # Its table and key has already been written, so just write out the contents.

        # Total number of vertices, integer.
        output.writeIndent('totalVertices = %i' % len(self.allVertices), indentOne)
        output.finishField()

        # Total number of UV channels/maps per vertex, integer.
        # Could also be the exact Love2D mesh format that should be used.
        output.writeIndent('totalUVLayers = %i' % self.totalUVLayers, indentOne)
        output.finishField()

        # Vertex data, array of floats. The data is interleaved ( see the OpenGL VBO docs: https://bit.ly/33QuixA ).
        # Vertex data is in this format:
        # (V1_X, V1_Y, V1_U1, V1_V1, V1_Un, V1_Vn... ) (V2_X, V2_Y, V2_U1, V2_V1, V2_Un, V2_Vn... ) (V3...)
        # The parenthesis above are just for clarification, the vertex data is flattened into a big list.
        # More attributes could be added besides the X,Y,U,V -- like R,G,B,A for color etc.
        output.writeIndent('vertices = ', indentOne)
        output.write(
            output.formatTable(
                # Unpack each element from each vertex output() iterable, for a completely flat array.
                chain.from_iterable(
                    chain(vertexData.outputCo, vertexData.uvs) for vertexData in self.allVertices
                )
            )
        )
        output.finishField()

        # Triangle index list, a flat integer array.
        # Every three integers form one triangle: A, B, C, | D, E, F, | ...
        output.writeIndent('triangles = ', indentOne)
        output.write(
            output.formatTable(
                # Add +1 to each vertex index to respect Love2D's 1-based vertex maps (index arrays).
                chain.from_iterable(
                    (triangleData[0]+1, triangleData[1]+1, triangleData[2]+1) for triangleData in self.allTriangles
                )
            )
        )
        output.finishField()

        # The string filename of the texture file, or nil.
        if self.texturePath:
            output.writeIndent('atlasTexture = "%s"' % self.texturePath, indentOne)
        else:
            output.writeIndent('atlasTexture = nil', indentOne)
        output.finishField()

        if settings.exportShapekeys and self.allShapekeys:
            output.writeIndent('shapeKeys = ', indentOne)
            output.write(
                output.formatTableVertical(
                    (
                        output.formatTableVertical(
                            (
                                'name = "%s"' % shapeName,
                                'offsets = ' + output.formatTable(map(output.formatTable, shapeData.offsets)),
                                'frames = ' + (output.formatTable(shapeData.frames)
                                               if (settings.exportAnimation and settings.exportShapekeyAnimation)
                                               else 'nil')
                            ),
                            indentThree
                        )
                        for shapeName, shapeData in self.allShapekeys.items()
                    ),
                    indentTwo
                )
            )
        else:
            output.writeIndent('shapeKeys = nil', indentOne)
        output.finishField()

        if settings.exportArmatures and self.armature:
            output.writeIndent('armature = "%s"' % self.armature.name, indentOne)
        else:
            output.writeIndent('armature = nil', indentOne)
        output.finishField()

        if settings.exportWeights != 'NONE' and self.allWeights:
            output.writeIndent('vertexWeights = ', indentOne)
            # Make a different generator depending on the 'exportWeights' setting.
            if settings.exportWeights == 'BONES' and self.armature:
                _deformBoneIndices = self.armature.deformBoneIndices
                _weightsGen = (
                    chain.from_iterable(
                        (_deformBoneIndices[groupName], weight)
                        for groupName, weight in weightData
                        if weight > 0.0 and (groupName in _deformBoneIndices)
                    )
                    for weightData in self.allWeights
                )
            else:
                _weightsGen = (
                    chain.from_iterable(
                        ('"'+groupName+'"', weight)
                        for groupName, weight in weightData if weight > 0.0
                    )
                    for weightData in self.allWeights
                )

            # Note: zero weights aren't exported, since they don't affect deformations.
            output.write(
                output.formatTable(
                    map(output.formatTable, _weightsGen)
                )
            )
        else:
            output.writeIndent('vertexWeights = nil', indentOne)


class ExportLUA(bpy.types.Operator, ExportHelper):
    """Exports the *visible* meshes and armatures to a Lua script, for use with Love2D"""
    bl_idname = "export_scene.lua"
    bl_label = 'Export LUA'
    bl_options = {'PRESET'}

    filename_ext = ".lua"
    filter_glob = bpy.props.StringProperty(default='*.lua', options={'HIDDEN'})

    # To be set in '_save()'.
    FLIP_Y = None
    V_COMPLEMENT = None

    exportMeshes = bpy.props.BoolProperty(
        name = 'Export Meshes',
        description = (
            'Exports meshes, with vertex positions, UVs etc. '
            'A reason to turn this off is if you only wanted bones to be exported (default: on)'
        ),
        default = True
    )
    exportWeights = bpy.props.EnumProperty(
        name = 'Vertex Weights',
        items = (
            ('NONE', 'None', 'Don\'t export vertex weights.'),
            ('BONES', 'Bone Weights', ('Only export weights from the vertex groups being used by armature bones.\n'
                                       'The weights on each mesh will use integer indices, indexing the bones of '
                                       'the armature that deforms the mesh.')),
            ('ALL', 'All Weights', ('Export all weights, from all vertex groups.\nVertex weights will use string '
                                    'names, instead of integer indices, to identify the vertex group they belong to.'))
        ),
        default = 'BONES'
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
            'Only export bones that have their Deform setting turned on. Keep this on if your armature rig '
            'is using non-Deform helper bones that don\'t need to be exported (default: on)'
        ),
        default = True
    )
    exportAnimation = bpy.props.BoolProperty(
        name = 'Export Animation',
        description = (
            'Export baked animation frames (default: on)'
        ),
        default = True
    )
    exportBoneAnimation = bpy.props.BoolProperty(
        name = 'Bone Animation',
        description = 'Bake frames for all bones (default: on)',
        default = True
    )
    exportShapekeyAnimation = bpy.props.BoolProperty(
        name = 'Shape Key Animation',
        description = 'Bake frames for the strength of shape keys (default: on)',
        default = True
    )
    topLeftCoordinates = bpy.props.BoolProperty(
        name = 'Top-left Coordinates',
        description = (
            'Export all coordinates with a flipped Y axis, so that the scene will look the same in the game engine '
            'as it does on the XY plane of the Top Ortho view of Blender (default: on)'
        ),
        default = True
    )


    def _gatherData(self, context):
        if not self.exportMeshes and not self.exportArmatures:
            self.report({'INFO'}, 'All object types disabled, nothing exported')
            return None

        # Set some helpers for mapping coordinates to be top-left friendly, if necessary.
        if self.topLeftCoordinates:
            ExportLUA.FLIP_Y = Matrix(((1.0, 0.0), (0.0, -1.0))) # Make a 2x2 matrix to flip the Y axis of 2D vectors.
            ExportLUA.V_COMPLEMENT = lambda uv: (uv[0], 1.0-uv[1]) # Used to remap the OpenGL V axis (in its UV space).
        else:
            ExportLUA.FLIP_Y = 1.0 # Use the scalar multiplication from the Vector class.
            ExportLUA.V_COMPLEMENT = lambda uv: uv # Pass-through, no changes.

        # =============================================================================
        # Gathering data from the scene into intermediary collections.
        # =============================================================================
        C = context
        exportableTypes = {'MESH', 'ARMATURE'}
        exportablesDict = dict()
        for obj in bpy.data.objects:
            if obj.is_visible(C.scene) and obj.type in exportableTypes and obj.name not in exportablesDict:

                if obj.type == 'MESH':
                    me = None
                    if self.exportMeshes:
                        me = MeshExportable(obj)
                        exportablesDict[obj.name] = me
                    # If the mesh is being deformed by an armature then add it to the export list already.
                    # This avoids a hidden armature affecting one or more meshes and not being exported at all.
                    if self.exportArmatures:
                        armatureObj = MeshExportable.findArmatureObject(obj)
                        if armatureObj:
                            if armatureObj.name not in exportablesDict:
                                ae = ArmatureExportable(armatureObj, self.exportDeformOnly)
                                exportablesDict[armatureObj.name] = ae
                            else:
                                ae = exportablesDict[armatureObj.name]
                            # MeshExportable needs a reference to the pose bones of the armature so it can
                            # index its bones, if 'exportWeights' is in 'BONES' mode.
                            if me and self.exportArmatures and self.exportWeights == 'BONES':
                                me.setArmatureExportable(ae)

                elif obj.type == 'ARMATURE' and self.exportArmatures:
                    exportablesDict[obj.name] = ArmatureExportable(obj, self.exportDeformOnly)

        # Update scene in case any armatures temporarily changed their pose modes.
        if self.exportArmatures:
            bpy.context.scene.update()

        if not exportablesDict:
            self.report({'INFO'}, 'No visible meshes or armatures, nothing exported')
            return None

        # Get the visible start and end frame numbers.
        if C.scene.use_preview_range:
            FRAME_START, FRAME_END = C.scene.frame_preview_start, C.scene.frame_preview_end
        else:
            FRAME_START, FRAME_END = C.scene.frame_start, C.scene.frame_end

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
            oldFrame = C.scene.frame_current
            for currentFrame in range(FRAME_START, FRAME_END + 1):
                C.scene.frame_set(currentFrame) # Thanks to MrRabbit (https://blender.stackexchange.com/a/133540/29992)
                C.scene.update()

                for exportable in exportablesDict.values():
                    exportable.sampleAnimationFrame()

                C.window_manager.progress_update(currentFrame)

                #sleep(0.1) # Ease the CPU on heavier scenes.

            C.window_manager.progress_end()
            C.scene.frame_set(oldFrame)
            C.scene.update()

        return tuple(exportablesDict.values())


    def _writeFile(self, exportables):
        # =============================================================================
        # Exporting to file.
        # The output is a Lua script that returns a table:
        #
        # return {
        #  (object data tables)
        # }
        # =============================================================================

        # Start the Lua script with a return of the main "scene" table.
        output = OutputWriter()
        output.write('return {\n')

        def __outputExportable(exportable):
            output.writeIndent('["%s"] = {\n' % exportable.name, 1)
            exportable.outputContents(output, indentOne=2, settings=self.properties)
            output.finishLine()
            output.writeIndent('}', 1)

        for index in range(len(exportables)-1):
            __outputExportable(exportables[index])
            output.finishField()
        __outputExportable(exportables[-1])

        # End the script by closing the scene table.
        output.write('\n}\n')

        # The path we're writing to is in the 'self.filepath' attribute that comes from the
        # bpy_extras.io_utils.ExportHelper class that this ExportLua operator class inherits, and was
        # automatically set after the user confirmed the file selection dialog.
        with open(self.filepath, 'w') as f:
            f.write(output.full())


    def draw(self, context):
        layout = self.layout

        col = layout.column(align=True)
        col.prop(self, 'exportMeshes', text='Export Meshes:')
        split = col.split(0.1, align=True)
        split.enabled = self.exportMeshes
        split.column()
        subCol = split.column(align=True)
        subCol.prop(self, 'exportShapekeys')
        subRow = subCol.row(align=True)
        subRow.alignment = 'LEFT'
        subSubCol = subRow.column(align=True)
        subSubCol.label('Vertex Weights:')
        subSubCol.prop(self, 'exportWeights', expand=True)

        layout.separator()

        col = layout.column(align=True)
        col.prop(self, 'exportArmatures')
        split = col.split(0.1, align=True)
        split.enabled = self.exportArmatures
        split.column()
        split.prop(self, 'exportDeformOnly')

        layout.separator()

        col = layout.column(align=True)
        col.prop(self, 'exportAnimation')
        split = col.split(0.1, align=True)
        split.column()
        subCol = split.column(align=True)
        subCol.enabled = self.exportAnimation
        subRow = subCol.row(align=True)
        subRow.enabled = self.exportArmatures
        subRow.prop(self, 'exportBoneAnimation')
        subRow = subCol.row(align=True)
        subRow.enabled = self.exportShapekeys
        subRow.prop(self, 'exportShapekeyAnimation')

        layout.separator()

        col = layout.column(align=True)
        col.label(text='Misc:')
        col.prop(self, 'topLeftCoordinates')


    def execute(self, context):
        # Make sure to work on Object mode to avoid surprises.
        self.restoreMode = (context.mode != 'OBJECT')
        if self.restoreMode:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=True)

        exportables = self._gatherData(context)

        if self.restoreMode:
            bpy.ops.object.mode_set(mode='OBJECT', toggle=True)

        if exportables:
            self._writeFile(exportables)
            return {'FINISHED'}
        else:
            return {'CANCELLED'}


def menuFuncExport(self, context):
    self.layout.operator(ExportLUA.bl_idname, text="Love2D Lua (.lua)")


classes = (
    ExportLUA,
)


def register():
    for cls in classes:
        bpy.utils.register_class(cls)

    bpy.types.INFO_MT_file_export.append(menuFuncExport)


def unregister():
    bpy.types.INFO_MT_file_export.remove(menuFuncExport)

    for cls in classes:
        bpy.utils.unregister_class(cls)


if __name__ == "__main__":
    register()
