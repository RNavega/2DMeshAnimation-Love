-- CPU-based (AKA "software") model skinning and shape keys.

love.window.setTitle(love.window.getTitle() .. ' (CPU)')

local modelLib = { }

local model = { }


function modelLib.loadModel()
    local data = love.filesystem.load('asset.lua')()
    
    modelData = data['MESH']
    
    model.mesh = love.graphics.newMesh(
        {
            {'VertexPosition', 'float', 2},
            {'VertexTexCoord', 'float', 2}
        },
        modelData.totalVertices,
        'triangles',
        'stream' -- Stream mode, mesh data changes on every frame.
    )
    -- The data file has X, Y, U1, V1 floats per vertex.
    local floatsPerVertex = 2 + 2 * modelData.totalUVLayers
    local dataVertices = modelData.vertices
    
    model.totalVertices = modelData.totalVertices
    model.baseVertices = { }
    model.dynamicVertices = { }
    for floatIndex = 1, (modelData.totalVertices * floatsPerVertex), floatsPerVertex do
        table.insert(
            model.dynamicVertices,
            {
                dataVertices[floatIndex],
                dataVertices[floatIndex+1],
                dataVertices[floatIndex+2],
                dataVertices[floatIndex+3]
            }
        )
        -- We keep a copy of the original vertex positions for easier reading in ':setFrame()'.
        -- The other original attributes aren't changed and don't need this treatment.
        table.insert(model.baseVertices, {dataVertices[floatIndex], dataVertices[floatIndex+1]})
    end
    model.mesh:setVertices(model.dynamicVertices)
    model.mesh:setVertexMap(modelData.triangles)

    -- The 'frame transform' of a bone is the (world-space) love.math.Transform that that bone should have
    -- on the current frame. We also need to cache the *inverse* of the transform of bones at bind pose
    -- (AKA rest pose, no animation) so that the skinning transform (what actually deforms vertices) can be
    -- computed as the result of (inverse-bind-transform * frame-transform).
    -- See more here: https://mmmovania.blogspot.com/2012/11/skeletal-animation-and-gpu-skinning.html
    model.bones = data[modelData.armature]
    model.totalBones = #model.bones
    model.boneFrameTransforms = { }
    model.boneBaseInvTransforms = { }
    model.boneTableTransforms = { }
    for boneIndex = 1, model.totalBones do
        local loveTransform = love.math.newTransform()
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
            -- The first 2 rows of a 4x4 matrix.
            model.boneTableTransforms, {
                1.0, 0.0, 0.0, 0.0,
                0.0, 1.0, 0.0, 0.0
            }
        ) 
    end

    -- Reformat the vertex weight data with keys, to be easier to query from Lua by using 'pairs()'.
    -- In the file, each array table has the same index as the vertex that that weight data belongs to.
    -- These array tables look like this : {BONE_INDEX_1, BONE_WEIGHT_1, BONE_INDEX_2, BONE_WEIGHT_2, ...}
    -- The hash tables we'll use look like this: {[BONE_INDEX_1]=BONE_WEIGHT_1, [BONE_INDEX_2]=BONE_WEIGHT2, ...}
    local keyedVertexWeights = { }
    for _, weights in ipairs(modelData.vertexWeights) do
        newWeights = { }
        for index = 1, #weights, 2 do
            -- Be aware if the bone indices in the model file uses zero-based indices, when you need to
            -- add +1 to be compatible with Lua tables.
            newWeights[weights[index]+1] = weights[index+1]
        end
        table.insert(keyedVertexWeights, newWeights)
    end
    model.vertexWeights = keyedVertexWeights

    model.shapeKeys = modelData.shapeKeys or { }
    model.totalShapeKeys = #model.shapeKeys
    model.shapekeyFrameValues = { }
    for shapeIndex = 1, model.totalShapeKeys do
        model.shapekeyFrameValues[shapeIndex] = 0.0
    end

    model.atlasTexture = modelData.atlasTexture
    if model.atlasTexture then
        model.image = love.graphics.newImage(modelData.atlasTexture)
        model.mesh:setTexture(model.image)
    end

    model.currentFrame = 1
    model.totalFrames = math.max(#model.shapekeyFrameValues, #model.bones[1].frames)
    model.frameAccumulator = 0.0
    
    return model
end


function model:setFrame(frame, showShapekeyResult, showBoneResult)   
    -- Update the Transform object of each bone for the given frame.
    
    local bones = self.bones
    local boneFrameTransforms = self.boneFrameTransforms
    local boneBaseInvTransforms = self.boneBaseInvTransforms
    local boneTableTransforms = self.boneTableTransforms
    
    for boneIndex = 1, self.totalBones do
        local bakedTransform = bones[boneIndex].frames[frame]
        local baseInvTable = boneBaseInvTransforms[boneIndex]        
        local boneTable = boneTableTransforms[boneIndex]
        
        -- Grab the first two rows of the 4x4 matrix result of (invBindPoseTransform * frameTransform).
        boneTable[1], boneTable[2], boneTable[3], boneTable[4],
        boneTable[5], boneTable[6], boneTable[7], boneTable[8] =
        boneFrameTransforms[boneIndex]:setTransformation(
            -- Position.
            bakedTransform[1],
            bakedTransform[2],
            -- Angle.
            bakedTransform[3],
            -- Scale.
            bakedTransform[4],
            bakedTransform[5],
            -- Bone origin, from the inverse table.
            baseInvTable[2],
            baseInvTable[3]
        ):apply(baseInvTable[1]):getMatrix()
    end
    
    -- Update the strength of shape key(s) for the given frame.
    
    local shapeKeys = self.shapeKeys
    local totalShapeKeys = self.totalShapeKeys
    local shapekeyFrameValues = self.shapekeyFrameValues
    
    for tableIndex = 1, totalShapeKeys do
        shapekeyFrameValues[tableIndex] = shapeKeys[tableIndex].frames[frame]
    end

    -- Process all vertices.
    
    local vertexWeights = self.vertexWeights
    local baseVertices = self.baseVertices
    local dynamicVertices = self.dynamicVertices
    
    for vertexIndex = 1, self.totalVertices do
        -- Start from the base (AKA bind pose) vertex position.
        local vTable = baseVertices[vertexIndex]
        local baseX = vTable[1]
        local baseY = vTable[2]

        -- Accumulate deformations from shape keys into baseX, baseY.
        if showShapekeyResult then
            for index = 1, totalShapeKeys do
                local position = shapeKeys[index].offsets[vertexIndex]
                local weight = shapekeyFrameValues[index]
                baseX = baseX + (position[1] * weight)
                baseY = baseY + (position[2] * weight)
            end
        end
        
        local finalX = 0.0
        local finalY = 0.0
        
        if showBoneResult then
            -- Deform the shape key result by only the bones influencing this vertex. The final vertex position
            -- is the weighted average of these bone deformations, with the weights being the skeleton skinning
            -- weights (assumed to be normalized).
            for boneIndex, weight in pairs(vertexWeights[vertexIndex]) do
                -- We manually multiply the bone skinning matrix with the vertex position in here
                -- because it measured faster than using the ':transformPoint()' function of Transform.
                -- We're inside a nested loop, so the cost of this function call is amplified.
                -- We would be calling it (totalVertices * totalBones) times per frame.
                local boneTable = boneTableTransforms[boneIndex]
                finalX = finalX + (boneTable[1]*baseX + boneTable[2]*baseY + boneTable[4]) * weight
                finalY = finalY + (boneTable[5]*baseX + boneTable[6]*baseY + boneTable[8]) * weight                
                -- The original way, transforming the vertex using the ':transformPoint()' method:
                --local deformedX, deformedY = boneFrameTransforms[boneIndex]:transformPoint(baseX, baseY)                
                --finalX = finalX + (deformedX * weight)
                --finalY = finalY + (deformedY * weight)
            end
        else
            finalX = baseX
            finalY = baseY
        end
        
        vTable = dynamicVertices[vertexIndex]
        vTable[1] = finalX
        vTable[2] = finalY
    end

    self.mesh:setVertices(dynamicVertices)
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


function model:draw()
    love.graphics.draw(self.mesh)
end


return modelLib
