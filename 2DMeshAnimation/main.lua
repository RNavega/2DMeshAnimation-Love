--[[
    Fast animated 2D mesh example using a skeleton.
    Assets and code by Rafael Navega, 2020.

    Thanks to TannerRogalsky for the reference:
    https://love2d.org/forums/viewtopic.php?t=83410
]]

io.stdout:setvbuf("no")

local FFI = require('ffi')
local SIZEOF_FLOAT = FFI.sizeof('float')

local model = {}

local showDebug = false


local function zeroIndexTable(t)
    local firstValue = t[1]
    table.remove(t, 1)
    t[0] = firstValue -- Becomes a hash key on LuaJIT ([0] = firstValue).
    return t
end


function love.load()
    local data = love.filesystem.load('character01-full-swim.lua')()

    model.totalVertices = data.totalVertices
    model.floatsPerVertex = 2 + 2 * data.totalUVLayers -- X, Y, (U1, V1, U2, V2 ...) per vertex.
    model.totalVertexFloats = data.totalVertices * model.floatsPerVertex
    model.baseVertices = zeroIndexTable(data.vertices)

    -- LÖVE Data object, used later with Mesh:setVertices().
    model.meshData = love.data.newByteData(model.totalVertexFloats * SIZEOF_FLOAT)
    
    -- Float pointer into 'model.meshData'. 0-index based.
    model.meshDataPointer = FFI.cast('float*', model.meshData:getFFIPointer())

    -- Copy all the contents of the 'model.baseVertices' array (including UV coordinates) to the mesh data pointer.
    -- Using a for loop measured faster than using FFI.copy().
    for floatIndex = 0, model.totalVertexFloats-1 do
        model.meshDataPointer[floatIndex] = model.baseVertices[floatIndex]
    end

    model.bones = data.bones
    model.totalBones = #data.bones
    
    model.vertexWeights = data.vertexWeights
    
    model.shapeKeys = data.shapeKeys or { }
    model.totalShapeKeys = #model.shapeKeys

    model.currentFrame = 1
    model.totalFrames = data.totalFrames

    model.mesh = love.graphics.newMesh(
        -- Mesh format according to the data in the file: 2 floats for a 2D position, 2 floats for UV coordinates.
        {{'VertexPosition', 'float', 2}, {'VertexTexCoord', 'float', 2}},
        data.totalVertices,
        'triangles',
        'dynamic'
    )
    model.mesh:setVertices(model.meshData)
    model.mesh:setVertexMap(data.triangles)

    model.atlasTexture = data.atlasTexture
    if model.atlasTexture then
        model.image = love.graphics.newImage(data.atlasTexture)
        model.mesh:setTexture(model.image)
    end
end


function model:setFrame(frame)
    local meshDataPointer = self.meshDataPointer
    local baseVertices = self.baseVertices
    local vertexWeights = self.vertexWeights
    
    local floatsPerVertex = self.floatsPerVertex
    
    local bones = self.bones

    local shapeKeys = self.shapeKeys
    local totalShapeKeys = self.totalShapeKeys
    
    local mathCos = math.cos
    local mathSin = math.sin

    for vertexIndex = 0, self.totalVertices-1 do
        local vertexFloatIndex = vertexIndex * floatsPerVertex
        
        -- Start from the base (aka "bind pose") vertex position.
        local baseX = baseVertices[vertexFloatIndex]
        local baseY = baseVertices[vertexFloatIndex+1]

        -- The final vertex position will be the sum of the results from all deformations.
        -- This works because the bone deformations are normalized, so they result in a full position.
        local finalX = 0.0
        local finalY = 0.0

        -- Accumulate deformations from shape keys into baseX, baseY before the bone deformations.
        -- **NOT USED** with the model provided in this demo, it doesn't have shape keys.
        for index = 1, totalShapeKeys do
            local shape = shapeKeys[index]
            local frameWeight = shape.frames[frame]
            local position = shape.positions[vertexIndex]
            baseX = baseX + (position[0] * frameWeight)
            baseY = baseY + (position[1] * frameWeight)
        end
        
        -- The shape key deformation results in baseX and baseY will then be deformed by the bone animation below.
        for boneIndex, weight in pairs(vertexWeights[vertexIndex+1]) do
            local bone = bones[boneIndex]
            local frameTransform = bone.frames[frame]

            local relativeX = (baseX - bone.originX) * frameTransform[2]
            local relativeY = (baseY - bone.originY) * frameTransform[3]

            local boneAngle = frameTransform[1]
            local cosAngle = mathCos(boneAngle)
            local sinAngle = mathSin(boneAngle)

            -- Affine transformation of a point.
            -- More info here: http://web.cse.ohio-state.edu/~parent.1/classes/581/Lectures/5.2DtransformsAhandout.pdf
            -- x' = x.cos + y.-sin + c
            -- y' = x.sin + y.cos  + f
            -- The redundant parenthesis below are just for readability.
            local deformedX = (relativeX * cosAngle) + (relativeY * -sinAngle) + frameTransform[4] + bone.originX
            local deformedY = (relativeX * sinAngle) + (relativeY *  cosAngle) + frameTransform[5] + bone.originY

            -- Do a weighted average into the final position.
            finalX = finalX + (deformedX * weight)
            finalY = finalY + (deformedY * weight)
        end

        -- Write to the FFI pointer of the LÖVE ByteData object.
        meshDataPointer[vertexFloatIndex]   = finalX
        meshDataPointer[vertexFloatIndex+1] = finalY
    end

    -- Update the mesh internals.
    self.mesh:setVertices(self.meshData)
end


function model:advanceFrame()
    self.currentFrame = self.currentFrame + 1
    if self.currentFrame > self.totalFrames then
        self.currentFrame = 1
    end
end


function love.update(dt)
    model:advanceFrame()
    model:setFrame(model.currentFrame)
    love.timer.sleep(0.03333333) -- Try to get about 30 FPS.
end


function love.draw()
    love.graphics.setWireframe(false)
    love.graphics.clear(0.0, 0.01, 0.05)
    love.graphics.setColor(1.0, 1.0, 1.0)

    love.graphics.print(string.format('Current frame: %d / %d', model.currentFrame, model.totalFrames), 10, 10)
    love.graphics.print('Hold any key to show debug info.', 10, 30)
    love.graphics.print('Press Esc or Alt+F4 to quit.', 10, 50)

    love.graphics.translate(love.graphics.getWidth() / 2, love.graphics.getHeight() / 2)

    love.graphics.setWireframe(false)
    love.graphics.draw(model.mesh)

    -- Debug information.
    if showDebug then
        love.graphics.setWireframe(true)
        model.mesh:setTexture()
        love.graphics.setColor(1.0, 1.0, 1.0, 0.33)
        love.graphics.draw(model.mesh, 0, 0)
        if model.atlasTexture then
            model.mesh:setTexture(model.image)
        end

        love.graphics.setWireframe(false)
        love.graphics.setColor(1.0, 0.0, 0.0)

        local BONE_LENGTH = 0.3
        for index = 1, model.totalBones do
            local boneData = model.bones[index]
            local tform = boneData.frames[model.currentFrame]
            local headX = boneData.originX + tform[4]
            local headY = boneData.originY + tform[5]
            local boneAimAngle = boneData.baseTransform[1] + tform[1] -- Add the delta angle to the base angle.
            local tailX = headX + math.cos(boneAimAngle) * BONE_LENGTH * tform[2]
            local tailY = headY + math.sin(boneAimAngle) * BONE_LENGTH * tform[3]
            love.graphics.circle('line', headX, headY, 6)
            love.graphics.line(headX, headY, tailX, tailY)
        end
    end
end


function love.keypressed(key)
    if key == 'escape' then
        if STOP then
            STOP = false
        else
            love.event.quit()
        end
        return
    else
        showDebug = true
    end
end


function love.keyreleased(key)
    showDebug = false
end
