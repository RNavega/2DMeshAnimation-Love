-- GPU-based model skinning and shape keys.

love.window.setTitle(love.window.getTitle() .. ' (GPU)')

local modelLib = { }

local model = { }


local vertexCode = [[
    const int TOTAL_BONES = 16;
    extern mat4 frameTransforms[TOTAL_BONES];

    const int TOTAL_SHAPEKEYS = 1;
    extern float frameShapekeyWeights[TOTAL_SHAPEKEYS];

    // DEBUG: debug uniforms, please ignore.
    extern bool showShapekeyResult;
    extern bool showBoneResult;

    // Vertex weights and bone indices: each vertex can be weighted to 4 bones at most
    // (but could be less). Each component of 'VertexBoneIndices' is the integral index
    // of the bone matrix in frameTransforms that manipulates this vertex. Each component
    // of 'VertexBoneWeights' is the respective weight of that deformation, when mixed
    // with that of the other bones.
    
    attribute vec4 VertexBoneIndices;
    attribute vec4 VertexBoneWeights;

    // Shape keys: one vec4 attribute can store two 2D shape-key deltas (a vec2 each) on a vertex.
    //
    // Note that the maximum attributes guaranteed by the OpenGL specification is 16. More than
    // that depends on the hardware. Love2D already uses some (search for 'VertexPosition' in here:
    // https://github.com/love2d/love/blob/master/src/modules/graphics/wrap_GraphicsShader.lua).
    // So the total available is: 16 - (attributes Love2D uses)
    
    attribute vec4 VertexShapekeys_A_B;
    // You then add more shape key attributes as needed: attribute vec4 VertexShapeKeys_C_D; etc.
    // Another possible way is assigning shape key indices to vertices in the same way as bones, so
    // that each vertex can be affected by different shape keys (instead of all shape keys at once).

    // Returns the position of the vertex after being affected by all shape keys available.
    vec2 shapeKeyResult(vec2 position)
    {
        return (
            position
            + (VertexShapekeys_A_B.xy * frameShapekeyWeights[0]) // This demo only uses one shape key.
          //+ (VertexShapekeys_A_B.zw * frameShapekeyWeights[1])
          //+ (VertexShapekeys_C_D.xy * frameShapekeyWeights[2])
          //+ (VertexShapekeys_C_D.zw * frameShapekeyWeights[3]) // etc.
        );
    }
    
    vec4 position(mat4 transform_projection, vec4 vertex_position)
    {
        // 1) Get the shape key result for the vertex.
        vec4 basePosition = vec4(shapeKeyResult(vertex_position.xy), 0.0, 1.0);

        // DEBUG:
        basePosition.xy = mix(vertex_position.xy, basePosition.xy, vec2(showShapekeyResult, showShapekeyResult));

        // 2) Deform the shape key result by the bones assigned to the vertex, supporting 4 bones at most.
        // This code assumes that the bone weights are normalized.
        vec2 final_position;
        final_position =  (frameTransforms[int(VertexBoneIndices[0])] * basePosition).xy * VertexBoneWeights[0];
        final_position += (frameTransforms[int(VertexBoneIndices[1])] * basePosition).xy * VertexBoneWeights[1];
        final_position += (frameTransforms[int(VertexBoneIndices[2])] * basePosition).xy * VertexBoneWeights[2];
        final_position += (frameTransforms[int(VertexBoneIndices[3])] * basePosition).xy * VertexBoneWeights[3];

        // DEBUG:
        final_position = mix(basePosition.xy, final_position, vec2(showBoneResult, showBoneResult));

        // 3) Transform the vertex like in the default Love2D vertex shader.
        return transform_projection * vec4(final_position, 0.0, 1.0);
    }]]


function modelLib.loadModel()
    local data = love.filesystem.load('asset.lua')()

    modelData = data['MESH']

    model.mesh = love.graphics.newMesh(
        {
            {'VertexPosition', 'float', 2},
            {'VertexTexCoord', 'float', 2},
            {'VertexBoneIndices', 'float', 4},
            {'VertexBoneWeights', 'float', 4},
            {'VertexShapekeys_A_B', 'float', 4}
        },
        modelData.totalVertices,
        'triangles',
        'static' -- Static, never changes. The deformation happens in the vertex shader.
    )
    -- The data file has X, Y, U1, V1 floats per vertex.
    local vertexDataFloats = 2 + 2 * modelData.totalUVLayers
    -- The Love2D mesh has X, Y, U1, V1, 4 bone indices, 4 bone weights per vertex, all floats.
    local formatFloats = vertexDataFloats + 4 + 4

    -- Copy all the contents of the vertices data (including UV coordinates and the bone indices
    -- and bone weights attributes) to the Love2D mesh.
    local tempVertices = { }
    local vertices = modelData.vertices
    local vertexWeights = modelData.vertexWeights
    local shapeKeys = modelData.shapeKeys or { }
    local totalShapeKeys = #shapeKeys
    local shapeKeyFloatsLeft = (totalShapeKeys % 2) * 2
    for vertexIndex = 1, modelData.totalVertices do
        local tableIndex = (vertexIndex-1) * vertexDataFloats + 1
        local weights = vertexWeights[vertexIndex]
        tempAttributes = {
            -- X, Y
            vertices[tableIndex],
            vertices[tableIndex+1],
            -- U1, V1
            vertices[tableIndex+2],
            vertices[tableIndex+3],
            -- (4) INDICES
            weights[1] or 0.0,
            weights[3] or 0.0,
            weights[5] or 0.0,
            weights[7] or 0.0,
            -- (4) WEIGHTS
            weights[2] or 0.0, -- Default to zero weight, no influence.
            weights[4] or 0.0,
            weights[6] or 0.0,
            weights[8] or 0.0,
            -- SHAPE KEY A, B, C etc.
            -- (Added below)
        }
        for shapeKeyIndex = 1, totalShapeKeys do
            local shapePosition = shapeKeys[shapeKeyIndex].offsets[vertexIndex]
            table.insert(tempAttributes, shapePosition[1])
            table.insert(tempAttributes, shapePosition[2])
        end
        if shapeKeyFloatsLeft > 0 then
            -- To complete the components of the vec4 attributes, if necessary.
            -- Shape key numbers are either even or odd, so add two extra components when odd.
            table.insert(tempAttributes, 0.0)
            table.insert(tempAttributes, 0.0)
        end
        table.insert(tempVertices, tempAttributes)
    end
    model.mesh:setVertices(tempVertices)
    model.mesh:setVertexMap(modelData.triangles)

    model.bones = data[modelData.armature]
    model.totalBones = #model.bones

    model.boneFrameTransforms = { }
    model.boneBaseInvTransforms = { }
    model.boneTableTransforms = { }
    for boneIndex = 1, model.totalBones do
        local baseTransform = model.bones[boneIndex].baseTransform
        local loveBaseTransform = love.math.newTransform(
            -- Position X, Y
            baseTransform[1], baseTransform[2],
            -- Angle.
            baseTransform[3],
            -- Scale X, Y
            baseTransform[4], baseTransform[5],
            -- Origin X, Y
            baseTransform[1], baseTransform[2]
        )
        table.insert(model.boneBaseInvTransforms, {loveBaseTransform:inverse(), baseTransform[1], baseTransform[2]})
        table.insert(model.boneFrameTransforms, love.math.newTransform())
        table.insert(
            model.boneTableTransforms,
            {
                1.0, 0.0, 0.0, 0.0,
                0.0, 1.0, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                0.0, 0.0, 0.0, 1.0
            }
        )
    end

    model.shapeKeys = shapeKeys
    model.totalShapeKeys = totalShapeKeys    
    model.shapekeyFrameValues = { }
    for shapeIndex = 1, totalShapeKeys do
        model.shapekeyFrameValues[shapeIndex] = 0.0
    end

    model.shader = love.graphics.newShader(nil, vertexCode)
    love.graphics.setShader(model.shader)

    model.atlasTexture = modelData.atlasTexture
    if model.atlasTexture then
        model.image = love.graphics.newImage(modelData.atlasTexture)
        model.mesh:setTexture(model.image)
    end

    model.currentFrame = 1
    model.totalFrames = #model.bones[1].frames
    model.frameAccumulator = 0.0

    return model
end


function model:setFrame(frame)
    local bones = self.bones
    local ftPointer = self.frameTransformsPointer
    local floatsPerTransform = self.floatsPerTransform
    local boneFrameTransforms = self.boneFrameTransforms
    local boneBaseInvTransforms = self.boneBaseInvTransforms
    local boneTableTransforms = self.boneTableTransforms

    for boneIndex = 1, self.totalBones do
        local bakedTransform = bones[boneIndex].frames[frame]
        local baseInvTable = boneBaseInvTransforms[boneIndex]

        --[[
            We copy several values at once to the frame transforms table, which will later be sent to the shader.

            For each bone, we only copy 12 of the 16 elements of the 4x4 matrix, because we initialized the final
            element in each matrix to '1.0' at load time, and this lets us avoid having to copy the final four
            values which are always (0.0, 0.0, 0.0, 1.0) in a 2D affine/orthogonal transform.
            
            All these 16 elements are from the 4x4 matrix of the 'bone transform for the current frame', made in
            this way: we take the world-space transform of the bone on this frame, then multiply it by the INVERSE
            world-space bind-pose transform of the same bone. So this 'frame transform' (AKA skinning transform)
            is relative to the bind-pose, and is used in the shader to deform vertices.
            See more here: https://mmmovania.blogspot.com/2012/11/skeletal-animation-and-gpu-skinning.html
        ]]
        local subT = boneTableTransforms[boneIndex]
        subT[1], subT[2], subT[3], subT[4],
        subT[5], subT[6], subT[7], subT[8],
        subT[9], subT[10], subT[11], subT[12] =
        boneFrameTransforms[boneIndex]:setTransformation(
            -- Position.
            bakedTransform[1],
            bakedTransform[2],
            -- Angle.
            bakedTransform[3],
            -- Scale.
            bakedTransform[4],
            bakedTransform[5],
            -- Bone origin.
            baseInvTable[2],
            baseInvTable[3]
        ):apply(baseInvTable[1]):getMatrix()
    end

    -- Also update the strength of shape key(s) for the current frame.
    local shapeKeys = self.shapeKeys
    local shapekeyFrameValues = self.shapekeyFrameValues
    for tableIndex = 1, self.totalShapeKeys do
        shapekeyFrameValues[tableIndex] = shapeKeys[tableIndex].frames[frame]
    end
end


function model:advanceFrame(dt, frameTime)
    self.frameAccumulator = self.frameAccumulator + dt

    if self.frameAccumulator >= frameTime then
        self.frameAccumulator = 0.0
        self.currentFrame = self.currentFrame + 1
        if self.currentFrame > self.totalFrames then
            self.currentFrame = 1
        end
    end
end


function model:draw(showShapekeyResult, showBoneResult)
    love.graphics.setShader(self.shader)
    self.shader:send('showShapekeyResult', showShapekeyResult)
    self.shader:send('showBoneResult', showBoneResult)
    self.shader:send('frameTransforms', unpack(self.boneTableTransforms))
    self.shader:send('frameShapekeyWeights', unpack(self.shapekeyFrameValues))
    love.graphics.draw(self.mesh)
    love.graphics.setShader(nil)
end


return modelLib
