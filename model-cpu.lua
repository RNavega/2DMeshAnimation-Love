-- CPU-based (AKA "software") model skinning and shape keys.

love.window.setTitle(love.window.getTitle() .. ' (CPU)')

local ffi = require('ffi')

local MATH_FLOOR = math.floor

local SIZEOF_FLOAT  = ffi.sizeof('float')
local FLOAT_PTR = ffi.typeof('float *')

local modelLib = { }

local model = { }


-- Creates a love.data.ByteData object and a C 'float' FFI pointer to it.
function modelLib.createFloatBuffer(totalUnits)
    local data = love.data.newByteData(totalUnits * SIZEOF_FLOAT)
    return data, ffi.cast(FLOAT_PTR, data:getFFIPointer())
end

-- Creates an FFI array of 'double' values. Using 'double' instead of 'float' because they work faster
-- (less conversions) with the LuaJIT FFI lib.
-- See the conversion tables for more info: https://luajit.org/ext_ffi_semantics.html#convert
-- 'totalUnits': either a table (whose length and values will be copied to the array), or a number
-- stating the desired size of the array.
function modelLib.doubleArray(totalUnits)
    if type(totalUnits) == 'table' then
        return ffi.new('double['..tostring(#totalUnits)..']', totalUnits)
    else
        return ffi.new('double['..tostring(totalUnits)..']')
    end
end


function modelLib.loadModel()
    local sceneData = love.filesystem.load('asset.lua')()
    local modelName, modelData = next(sceneData.meshes)

    model.mesh = love.graphics.newMesh(
        {
            {'VertexPosition', 'float', 2},
            {'VertexTexCoord', 'float', 2}
        },
        modelData.totalVertices,
        'triangles',
        'stream' -- Stream mode, mesh data changes on every frame.
    )
    -- The vertex format above has X, Y, U1, V1, so 4 floats per vertex.
    model.floatsPerVertex = 4
    model.totalVertices = modelData.totalVertices

    -- Create some FFI objects.
    -- A love.data.ByteData object and its (float) pointer, for storing vertex attributes.
    -- This is used with the Data argument overload of mesh:setVertices(), which profiled faster than
    -- the "Lua table of tables" overload.
    -- Löve seems to expect the C 'float' datatype when using mesh:setVertices() with this Data object overload.
    -- If you use 'double' the mesh comes out wrong.
    local bufferSize = modelData.totalVertices * model.floatsPerVertex
    model.vertexBuffer, model.vertexPointer = modelLib.createFloatBuffer(bufferSize)

    -- An array with flat vertex positions (x1, y1, x2, y2, ...). See modelLib.doubleArray() for more info.
    -- This will store the base positions of vertices, AKA their bind-pose positions, used in
    -- model:setFrame() as the initial vertex position before any deformations.
    model.baseArray = modelLib.doubleArray(modelData.totalVertices * 2)

    -- The vertex format being used has the attributes interleaved like this:
    -- (X1, Y1, U1_1, V1_1, ..., X2, Y2, U2_1, V2_1, ...). The sample mesh only has one UV set.
    local positionsSource = modelData.attributes.positions
    local uv1Name, uv1Source = next(modelData.attributes.uvs)
    -- Read these vertex attributes from the model data into the arrays.
    for zeroVertexIndex = 0, model.totalVertices-1 do
        local bufferIndex = zeroVertexIndex * model.floatsPerVertex
        local positionArrayIndex = zeroVertexIndex * 2
        local positionTableIndex = positionArrayIndex + 1
        model.vertexPointer[bufferIndex]   = positionsSource[positionTableIndex]   -- X
        model.vertexPointer[bufferIndex+1] = positionsSource[positionTableIndex+1] -- Y
        model.vertexPointer[bufferIndex+2] = uv1Source[positionTableIndex]         -- U1
        model.vertexPointer[bufferIndex+3] = uv1Source[positionTableIndex+1]       -- V1
        -- (...) U2, V2, R, G, B, A etc.

        model.baseArray[positionArrayIndex]   = positionsSource[positionTableIndex]
        model.baseArray[positionArrayIndex+1] = positionsSource[positionTableIndex+1]
    end
    model.mesh:setVertices(model.vertexBuffer)
    -- Remap the zero-based vertex indices in the triangle list to the 1-based indices that
    -- LÖVE's mesh:setVertexMap() expects.
    local tempTriangles = { }
    for triangleIndex = 1, #modelData.triangles do
        tempTriangles[triangleIndex] = modelData.triangles[triangleIndex] + 1
    end
    model.mesh:setVertexMap(tempTriangles)

    -- Armature (or skeleton), used for mesh skinning.
    model.armatureName = modelData.armatureName

    local armatureName, armatureData = next(sceneData.armatures)
    if model.armatureName ~= armatureName then
        error('Model "'..modelName..'" does not use armature "'..armatureName..'"')
    end
    -- Store only the armature data that the model cares about: the bones for which the mesh has
    -- vertex groups and their transforms (at bind pose and frame poses).
    local bindData = {frameArrays={}, invBaseTransforms={}, tempTransform=love.math.newTransform(),
                      debugTransforms={}}

    -- Associate each vertex group with the armature bone that it's holding vertex weights for.
    -- Fill the model.armature table with data used for deforming vertices.
    local zeroBindIndex = 0
    local perVertexWeights = { }
    local tempTransform = bindData.tempTransform
    for groupName, groupData in pairs(modelData.vertexGroups) do
        if groupData.isBone and armatureData.bones[groupName] then
            local boneData = armatureData.bones[groupName]
            -- Store the flat frames table for this bound bone.
            -- For some strange reason this profiled faster with a flat Lua table than an FFI array.
            --bindData.frameArrays[zeroBindIndex] = modelLib.doubleArray(boneData.frames)
            bindData.frameArrays[zeroBindIndex] = boneData.frames

            -- Store the inverse bind/base transform of this bone.
            tempTransform:setTransformation(
                -- Position X, Y
                boneData.baseTransform[1], boneData.baseTransform[2],
                -- Angle.
                boneData.baseTransform[3],
                -- Scale X, Y
                boneData.baseTransform[4], boneData.baseTransform[5]
            )
            bindData.invBaseTransforms[zeroBindIndex] = tempTransform:inverse()
            -- Only used in main.lua for drawing the disabled bones. Can be removed in production.
            bindData.debugTransforms[zeroBindIndex] = tempTransform:clone()

            -- Add the vertex weights from this group to the per-vertex weights table.
            -- The weight data in a vertex group is a flat list of
            -- (zeroVertexIndex, weight) pairs. The perVertexWeights table will map
            -- **zero-based** vertex indices to flat tables of (zeroBindIndex, weight)
            -- pairs, one table for each vertex with the bind pairs for it.
            if groupData.weights then
                for weightPairIndex = 1, #groupData.weights, 2 do
                    local zeroVertexIndex = groupData.weights[weightPairIndex]
                    local weight = groupData.weights[weightPairIndex+1]
                    local indexWeightPair = {zeroBindIndex, weight}
                    if perVertexWeights[zeroVertexIndex] then
                        table.insert(perVertexWeights[zeroVertexIndex], indexWeightPair)
                    else
                        perVertexWeights[zeroVertexIndex] = {indexWeightPair}
                    end
                end
            end
            -- Advance the bind index, aligned to the armature lists above.
            -- It represents only the bones used by the mesh, that is, only
            -- the bones that have vertex groups on the mesh.
            zeroBindIndex = zeroBindIndex + 1
        end
    end
    bindData.totalBinds = zeroBindIndex
    -- A flat FFI array holding the computed skinning transform for the current frame
    -- of the bones deforming the mesh.
    -- Only some elements of the matrix are stored (xx, xy, tx and yx, yy, ty, totaling
    -- 6 elements, as explained in model:setFrame()). So that's 6 elements per bound bone.
    bindData.tempTransforms = modelLib.doubleArray(bindData.totalBinds * 6)

    -- Use the per-vertex weights table to build flat FFI arrays for fast weights access.
    -- This impacts a lot, since it's heavily used in the deformation code in a nested loop in
    -- model:setFrame().
    -- We make a pair of arrays:
    -- In the flat weight indices array, it maps the (zero-based) vertex index to the index in
    -- the flat 'vertex weight pairs array' where the data relative to that vertex starts.
    -- In the flat 'weight pairs array', each vertex is represented by variable-sized data:
    -- [count1, indexA, weightA, indexB, weightB, ...]
    --
    -- The 'count#' is the total amount of 'index#' and 'weight#' elements found ahead (not pairs
    -- but individual elements). This count can be different for each vertex, like many elements
    -- for one vertex while zero elements for another.
    --
    -- The 'index#' and 'weight#' values identify the bone deforming that vertex with that weight.

    bindData.weightIndicesArray = modelLib.doubleArray(model.totalVertices)
    local dataStartIndex = 0
    local flatWeightsTable = { }

    for zeroVertexIndex = 0, model.totalVertices-1 do
        bindData.weightIndicesArray[zeroVertexIndex] = dataStartIndex
        local vertexWeightPairs = perVertexWeights[zeroVertexIndex]
        if vertexWeightPairs then
            local totalElementsAhead = #vertexWeightPairs * 2
            -- Store the flat number of elements found ahead. This is what
            -- the indices in weightIndicesArray point to.
            table.insert(flatWeightsTable, totalElementsAhead)
            for _, weightPair in ipairs(vertexWeightPairs) do
                -- Store the index to the bone skinning transform. Since a skinning transform
                -- has 6 elements, we step the index by this much.
                -- Also store the bone weight.
                table.insert(flatWeightsTable, weightPair[1] * 6)
                table.insert(flatWeightsTable, weightPair[2])
            end
            dataStartIndex = dataStartIndex + totalElementsAhead
        else
            -- No weights for this vertex, so add a count of zero.
            table.insert(flatWeightsTable, 0)
        end
        -- Advance the index to the start of the data for the next vertex.
        dataStartIndex = dataStartIndex + 1
    end
    -- Create the flat FFI weights array, initializing from a table.
    -- This table uses 1-based Lua indices, but the FFI lib will copy them to index [0]
    -- of the FFI array.
    bindData.weightPairsArray = modelLib.doubleArray(flatWeightsTable)

    model.bindData = bindData

    -- Load all shape keys. The sample model only comes with one.
    -- We store a table of flat FFI arrays holding the offsets for each vertex, as well as
    -- a table holding flat FFI arrays with the strength of the shape key on each frame and
    -- a flat FFI array with the shape key strengths for the current frame (each index
    -- maps to the strength of a certain shape key).
    local shapekeys = {offsetArrays={}, frameArrays={}, tempStrengths=nil}

    local zeroShapeIndex = 0
    for shapeName, shapeData in pairs(modelData.shapeKeys) do
        shapekeys.offsetArrays[zeroShapeIndex] = modelLib.doubleArray(shapeData.offsets)
        shapekeys.frameArrays[zeroShapeIndex] = modelLib.doubleArray(shapeData.frames)
        zeroShapeIndex = zeroShapeIndex + 1
    end
    shapekeys.total = zeroShapeIndex
    -- The temporary strengths array will hold the shape key strengths for the current frame, to be
    -- sampled by each vertex.
    shapekeys.tempStrengths = modelLib.doubleArray(shapekeys.total)

    model.shapekeys = shapekeys

    model.imagePath = modelData.texture
    if model.imagePath then
        model.image = love.graphics.newImage(model.imagePath)
        model.mesh:setTexture(model.image)
    end

    model.currentFrame = 1.0
    if sceneData.animation then
        model.totalFrames = sceneData.animation.length
        model.fps = sceneData.animation.fps or 30.0
    else
        model.totalFrames = 0
        model.fps = 30.0
    end

    return model
end


function model:setFrame(frameA, frameB, alpha, showShapekeyResult, showBoneResult)
    -- This function works in two parts:
    --
    -- 1) Updating the skinning transform of all bones for the current frame, as well
    --    as copying the strengths of all shape keys for the current frame into a
    --    scratchpad flat array to be consulted by vertices.
    --
    -- 2) Deform vertices by first accumulating shape key deformations, and then bone
    --    deformations on top of those.

    local frameArrays = self.bindData.frameArrays
    local tempTFs = self.bindData.tempTransforms
    local tempTransform = self.bindData.tempTransform
    local invBaseTransforms = self.bindData.invBaseTransforms

    if frameA == 0 or frameB == 0 then
        error(string.format('model:setFrame() -> Frame interval out of bounds (%d and %d)', frameA, frameB))
    end

    -- The integer frames will be used to sample the flat bone frames table, which contains
    -- values for use with love.math.Transform's: (positionX, positionY, angle, scaleX, scaleY).
    -- There are 5 values total per frame, so use that as stride.
    local transformAIndex = (frameA - 1) * 5 + 1
    local transformBIndex = (frameB - 1) * 5 + 1

    -- Caching these function references for some really small improvement, almost not worth it.
    local tempSetTransformation = tempTransform.setTransformation
    local tempApply = tempTransform.apply
    local tempGetMatrix = tempTransform.getMatrix

    for zeroBindIndex = 0, self.bindData.totalBinds-1 do
        -- Grab the first two rows of the 4x4 matrix result of (invBindPoseTransform * frameTransform)
        -- to be stored in the model.tempTransforms flat array and later sampled by vertices.
        --
        -- The 4x4 matrix has its rows like this, formed by a 3x3 rotation-and-scale matrix, and the
        -- 1x3 translation column:
        -- xx, xy, xz, tx
        -- yx, yy, yz, ty
        -- zx, zy, zz, tz
        --  0,  0,  0,  1
        --
        -- For a 2D affine transform with bones we need a 2x2 matrix plus the translation, so it's
        -- the (xx, xy, tx) elements and the (yx, yy, ty) elements. Elements xz and yz are unused.

        local frameTF = frameArrays[zeroBindIndex]
        local tfIndex = zeroBindIndex * 6
        local unusedElement
        tempTFs[tfIndex],   tempTFs[tfIndex+1], unusedElement, tempTFs[tfIndex+2],
        tempTFs[tfIndex+3], tempTFs[tfIndex+4], unusedElement, tempTFs[tfIndex+5] =
        tempGetMatrix(tempApply(tempSetTransformation(tempTransform,
            -- Interpolated position X, Y.
            frameTF[transformAIndex]   + (frameTF[transformBIndex]  -frameTF[transformAIndex]  )*alpha,
            frameTF[transformAIndex+1] + (frameTF[transformBIndex+1]-frameTF[transformAIndex+1])*alpha,
            -- Interpolated angle.
            frameTF[transformAIndex+2] + (frameTF[transformBIndex+2]-frameTF[transformAIndex+2])*alpha,
            -- Interpolated scale X, Y.
            frameTF[transformAIndex+3] + (frameTF[transformBIndex+3]-frameTF[transformAIndex+3])*alpha,
            frameTF[transformAIndex+4] + (frameTF[transformBIndex+4]-frameTF[transformAIndex+4])*alpha
        ), invBaseTransforms[zeroBindIndex]))
    end

    -- Update the strength of shape key(s) for the given frame.

    local totalShapeKeys = self.shapekeys.total
    local shapeKeyOffsetArrays = self.shapekeys.offsetArrays
    local shapeKeyFrameArrays = self.shapekeys.frameArrays
    local tempShapeKeyStrengths = self.shapekeys.tempStrengths

    local zeroFrameA = frameA - 1
    local zeroFrameB = frameB - 1

    for zeroShapeIndex = 0, totalShapeKeys-1 do
        -- Interpolated shape key strength.
        local shapeFrames = shapeKeyFrameArrays[zeroShapeIndex]
        tempShapeKeyStrengths[zeroShapeIndex] = (shapeFrames[zeroFrameA] +
                                                 (shapeFrames[zeroFrameB]-shapeFrames[zeroFrameA]) * alpha)
    end

    -- Process all vertices.

    local vertexPointer = self.vertexPointer
    local fpv = self.floatsPerVertex
    local baseArray = self.baseArray

    local weightIndicesArray = model.bindData.weightIndicesArray
    local weightPairsArray = model.bindData.weightPairsArray

    for zeroVertexIndex = 0, self.totalVertices-1 do
        -- Start from the base (AKA bind pose) vertex position.
        local positionIndex = zeroVertexIndex * 2
        local baseX = baseArray[positionIndex]
        local baseY = baseArray[positionIndex+1]

        -- Accumulate deformations from shape keys.
        if showShapekeyResult then
            for zeroShapeIndex = 0, totalShapeKeys-1 do
                local weight = tempShapeKeyStrengths[zeroShapeIndex]
                local offsetsArray = shapeKeyOffsetArrays[zeroShapeIndex]
                baseX = baseX + (offsetsArray[positionIndex] * weight)
                baseY = baseY + (offsetsArray[positionIndex+1] * weight)
            end
        end

        local finalX = 0.0
        local finalY = 0.0

        if showBoneResult then
            -- Deform the shape key result by only the bones influencing this vertex.
            -- The final vertex position is the shape key result plus the weighted average of
            -- these bone deformations, with these weights being the skeleton skinning weights
            -- (assumed to be normalized).
            local weightIndex = weightIndicesArray[zeroVertexIndex]
            for i = 1, weightPairsArray[weightIndex], 2 do
                local tfIndex = weightPairsArray[weightIndex+i]
                local weight = weightPairsArray[weightIndex+i+1]
                -- We manually multiply the bone skinning (AKA deform) matrices with the vertex position
                -- in here because it profiled faster than using the ':transformPoint()' function of Transform.
                -- We're inside a nested loop, so the cost of that function call would've been amplified.
                finalX = finalX + (  tempTFs[tfIndex]*baseX + tempTFs[tfIndex+1]*baseY + tempTFs[tfIndex+2]) * weight
                finalY = finalY + (tempTFs[tfIndex+3]*baseX + tempTFs[tfIndex+4]*baseY + tempTFs[tfIndex+5]) * weight
            end
        else
            finalX = baseX
            finalY = baseY
        end
        local pointerIndex = zeroVertexIndex * fpv
        vertexPointer[pointerIndex]   = finalX
        vertexPointer[pointerIndex+1] = finalY
    end
    self.mesh:setVertices(self.vertexBuffer)
end


function model:advanceFrame(frameDT, showShapekeyResult, showBoneResult)
    local nextFrame = self.currentFrame + (frameDT * self.fps)

    -- Get the LERP factor for interpolating between the current frame and the next.
    local nextFrameInt = MATH_FLOOR(nextFrame)
    local lerpFactor = nextFrame - nextFrameInt

    -- Wrap the frame counter if necessary (using 1-based indices, so it wraps after totalFrames + 1).
    -- Also, find out what neighboring integer frames A and B to send to the mesh deformation function.
    -- The function takes the shape key and bone deformations between the two frames and interpolates
    -- them using the amount of time passed since the last update (frameDT, in seconds).
    local frameA, frameB
    if nextFrame > (self.totalFrames + 1) then
        frameA = 1
        frameB = 2
        -- Wrapping the frame counter.
        nextFrame = 1
    else
        frameA = nextFrameInt
        if nextFrameInt == self.totalFrames then
            -- Wrap the integer frame B, as it should always be one step ahead of frame A.
            frameB = 1
        else
            frameB = nextFrameInt + 1
        end
    end

    -- The call below was being used to time this deformation function, to see if any changes
    -- were actually improving things.
    --self:setFrameDebug(frameA, frameB, lerpFactor, showShapekeyResult, showBoneResult)
    self:setFrame(frameA, frameB, lerpFactor, showShapekeyResult, showBoneResult)

    self.currentFrame = nextFrame
end


function model:setFrameDebug(frameA, frameB, lerpFactor, showShapekeyResult, showBoneResult)
    local timings = {}
    love.timer.sleep(2.0)
    love.timer.step()
    for x = 1, 20 do
        for i = 1, 1000 do
            self:setFrame(frameA, frameB, lerpFactor, showShapekeyResult, showBoneResult)
        end
        timings[x] = love.timer.step()
    end
    error('time: '..tostring(math.min(unpack(timings))))
end


function model:draw()
    love.graphics.draw(self.mesh)
end


return modelLib
