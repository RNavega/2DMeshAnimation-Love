--[[
    Fast animated 2D mesh using skeletal and shape key (AKA morph target) deformations.
    Assets and code by Rafael Navega, 2020.
    Version 1.1.0.
]]

io.stdout:setvbuf("no")


--[[
    Choice of CPU or GPU skinning:
    Uncomment the line below that loads the module that you want to test, and comment the other one out.
]]
local modelLib = require('model-cpu') -- CPU / software skinning.
--local modelLib = require('model-gpu') -- GPU skinning. On my system this is about 9x faster than 'model-cpu'.


local FRAME_TIME = 1.0 / 30.0

local model = {}

local showDebug = false
local showBoneResult = true
local showShapekeyResult = true


function love.load()
    model = modelLib.loadModel()
end


function love.update(dt)
    model:advanceFrame(dt, FRAME_TIME)
    model:setFrame(model.currentFrame, showShapekeyResult, showBoneResult)
end


function love.draw()
    love.graphics.origin()
    love.graphics.setWireframe(false)
    love.graphics.clear(0.0, 0.01, 0.05)
    love.graphics.setColor(1.0, 1.0, 1.0)

    love.graphics.print(string.format('Current frame: %d / %d', model.currentFrame, model.totalFrames), 10, 10)
    love.graphics.print(
        'Hold Left to disable shape key influence. ' .. ((showShapekeyResult and '(Shape keys ON)') or '(Shape keys OFF)'),
        10, 30
    )
    love.graphics.print(
        'Hold Right to disable bone influence. ' .. ((showBoneResult and '(Bones ON)') or '(Bones OFF)'), 10, 50
    )
    love.graphics.print('Hold any other key to show debug info. ' .. ((showDebug and '(ON)') or '(OFF)'), 10, 70)
    love.graphics.print('Press Esc or Alt+F4 to quit.', 10, 90)

    love.graphics.translate(love.graphics.getWidth() / 2, love.graphics.getHeight() / 2)
    
    -- When using module 'model-gpu' this draw function also sends the shader uniform data.
    model:draw(showShapekeyResult, showBoneResult)

    -- Debug information.
    if showDebug then
        love.graphics.setWireframe(true)
        model.mesh:setTexture()
        love.graphics.setColor(1.0, 1.0, 1.0, 0.33)
        model:draw(showShapekeyResult, showBoneResult)
        model.mesh:setTexture(model.image)

        love.graphics.setWireframe(false)
        love.graphics.setColor(1.0, 0.0, 0.0)

        local BONE_LENGTH = 32.0
        for index = 1, model.totalBones do
            local boneData = model.bones[index]
            local tform = (showBoneResult and boneData.frames[model.currentFrame]) or boneData.baseTransform
            local headX = tform[1]
            local headY = tform[2]
            local boneAimAngle = tform[3]
            local tailX = headX + math.cos(boneAimAngle) * BONE_LENGTH
            local tailY = headY + math.sin(boneAimAngle) * BONE_LENGTH
            love.graphics.circle('line', headX, headY, 6)
            love.graphics.line(headX, headY, tailX, tailY)
        end
    end
end


function love.keypressed(key)
    if key == 'escape' then
        love.event.quit()
    elseif key == 'left' then
        showShapekeyResult = false
    elseif key == 'right' then
        showBoneResult = false
    else
        showDebug = true
    end
end


function love.keyreleased(key)
    if key == 'left' then
        showShapekeyResult = true
    elseif key == 'right' then
        showBoneResult = true
    else
        showDebug = false
    end
end


-- Custom love.run with frame limiting.
function love.run()
    if not love.timer or not love.event or not love.graphics then
        error('Modules not enabled')
    end

	if love.load then love.load(love.arg.parseGameArguments(arg), arg) end

	-- Main loop.
	return function()
        local timerStep = love.timer.step
        local timerSleep = love.timer.sleep
        local eventPump = love.event.pump
        local eventPoll = love.event.poll
        local eventHandlers = love.handlers
        local dt1 = 0.0
        local dt2 = 0.0
        local sleepTime = 0.0
    
        while true do
            eventPump()
            for name, a,b,c,d,e,f in eventPoll() do
                if name == "quit" then
                    if not love.quit or not love.quit() then
                        return a or 0
                    end
                end
                eventHandlers[name](a,b,c,d,e,f)
            end

            -- Call update and draw.
            dt1 = timerStep()
            love.update(dt1+dt2)

            if love.graphics.isActive() then
                love.draw()
                love.graphics.present()
            end
            
            dt2 = timerStep()            
            sleepTime = FRAME_TIME - (dt1+dt2)
            if sleepTime > 0.0 then
               timerSleep(sleepTime)
            end
        end
	end
end
