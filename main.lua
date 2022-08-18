--[[
    Fast animated 2D mesh using skeletal and shape key (AKA morph target) deformations.
    Assets and code by Rafael Navega, 2022.
    Version 1.2.0 (2022-08-18)

    LICENSE: Public Domain.
]]

io.stdout:setvbuf("no")


local modelLib = require('model-cpu') -- CPU / software skinning.

local model

local showDebug = false
local showBoneResult = true
local showShapekeyResult = true

local tempTransform


function love.load()
    model = modelLib.loadModel()
    tempTransform = love.math.newTransform()
end


function love.update(dt)
    model:advanceFrame(dt, showShapekeyResult, showBoneResult)
end


function love.draw()
    love.graphics.origin()
    love.graphics.setWireframe(false)
    love.graphics.clear(0.0, 0.01, 0.05)
    love.graphics.setColor(1.0, 1.0, 1.0)

    love.graphics.print(string.format('Current frame: %.3f / %.1f', model.currentFrame, model.totalFrames),
                        10, 10)
    love.graphics.print('Hold Left to disable shape key influence. (Shape keys '..
                        (showShapekeyResult and 'ON)' or 'OFF)'), 10, 30)
    love.graphics.print('Hold Right to disable bone influence. (Bones '..
                        (showBoneResult and 'ON)') or 'OFF)', 10, 50)
    love.graphics.print('Hold Down to slow down the model framerate. (Frame rate: '..
                        tostring(model.fps)..')', 10, 70)
    love.graphics.print('Hold any other key to show debug info. ('..
                        (showDebug and 'ON)' or 'OFF)'), 10, 90)
    love.graphics.print('Press Esc or Alt+F4 to quit.', 10, 110)

    -- Draw the animated model.
    love.graphics.translate(love.graphics.getWidth() / 2, love.graphics.getHeight() / 2)
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
        for zeroBindIndex = 0, model.bindData.totalBinds-1 do
            if showBoneResult then
                love.graphics.origin()
                local frameTF = model.bindData.frameArrays[zeroBindIndex]
                local tfIndex = (math.floor(model.currentFrame) - 1) * 5 + 1
                tempTransform:setTransformation(frameTF[tfIndex], frameTF[tfIndex+1],
                                            frameTF[tfIndex+2],
                                            frameTF[tfIndex+3], frameTF[tfIndex+4])
            else
                love.graphics.origin()
                tempTransform:reset()
                tempTransform:apply(model.bindData.debugTransforms[zeroBindIndex])
            end
            love.graphics.translate(love.graphics.getWidth() / 2, love.graphics.getHeight() / 2)
            love.graphics.applyTransform(tempTransform)
            love.graphics.circle('line', 0, 0, 6)
            love.graphics.line(0, 0, BONE_LENGTH, 0)
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
    elseif key == 'down' then
        model.fps = 6.0
    else
        showDebug = true
    end
end


function love.keyreleased(key)
    if key == 'left' then
        showShapekeyResult = true
    elseif key == 'right' then
        showBoneResult = true
    elseif key == 'down' then
        model.fps = 30.0
    else
        showDebug = false
    end
end


-- Custom love.run.
function love.run()
    if not (love.timer and love.event and love.graphics) then
        error('LÖVE modules not enabled')
    end

    if love.load then love.load(love.arg.parseGameArguments(arg), arg) end

        -- Main loop.
    return function()
        local timerStep = love.timer.step
        local eventPump = love.event.pump
        local eventPoll = love.event.poll
        local eventHandlers = love.handlers
        local loveUpdate = love.update
        local loveDraw = love.draw
        local graphicsPresent = love.graphics.present

        timerStep()

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
            -- Frame limiting is done by the VSYNC option set in conf.lua.
            loveUpdate(timerStep())
            loveDraw()
            graphicsPresent()
        end
    end
end
