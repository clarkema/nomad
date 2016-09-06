local mash = {"cmd", "alt", "ctrl"}

hs.hotkey.bind(mash, "W", function()
    hs.notify.new({title="Hammerspoon", informativeText="Hello"}):send()
end)

hs.hotkey.bind(mash, ".", hs.hints.windowHints)
hs.window.animationDuration = 0


hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Left", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Right", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

hs.hotkey.bind({"cmd", "alt", "ctrl"}, "Up", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w
  f.h = max.h
  win:setFrame(f)
end)

-- local caffeine = hs.menubar.new()
-- function setCaffeineDisplay(state)
--     if state then
--         caffeine:setTitle("AWAKE")
--     else
--         caffeine:setTitle("SLEEPY")
--     end
-- end
-- 
-- function caffeineClicked()
--     setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
-- end
-- 
-- if caffeine then
--     caffeine:setClickCallback(caffeineClicked)
--     setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
-- end

-- This section stolen from
-- https://github.com/cmsj/hammerspoon-config/blob/master/init.lua
--
-- Capture the hostname, so we can make this config behave differently across
-- my Macs
hostname = hs.host.localizedName()

-- Draw little text/dot pairs in the bottom right corner of the primary display, to indicate firewall/backup status of my machine
function renderStatuslets()
    --if (hostname ~= "odin") then
        --return
    --end
    -- Destroy existing Statuslets
    if firewallStatusText then firewallStatusText:delete() end
    if firewallStatusDot then firewallStatusDot:delete() end
    if cccStatusText then cccStatusText:delete() end
    if cccStatusDot then cccStatusDot:delete() end
    if arqStatusText then arqStatusText:delete() end
    if arqStatusDot then arqStatusDot:delete() end

    -- Defines for statuslets - little coloured dots in the corner of my screen that give me status info, see:
    -- https://www.dropbox.com/s/3v2vyhi1beyujtj/Screenshot%202015-03-11%2016.13.25.png?dl=0
    local initialScreenFrame = hs.screen.allScreens()[1]:fullFrame()

    -- Start off by declaring the size of the text/circle objects and some anchor positions for them on screen
    local statusDotWidth = 10
    local statusTextWidth = 30
    local statusTextHeight = 15
    local statusText_x = initialScreenFrame.x + initialScreenFrame.w - statusDotWidth - statusTextWidth
    local statusText_y = initialScreenFrame.y + initialScreenFrame.h - statusTextHeight
    local statusDot_x = initialScreenFrame.x + initialScreenFrame.w - statusDotWidth
    local statusDot_y = statusText_y

    -- Now create the text/circle objects using the sizes/positions we just declared (plus a little fudging to make it all align properly)
    firewallStatusText = hs.drawing.text(hs.geometry.rect(statusText_x + 5,
                                                          statusText_y - (statusTextHeight*2) + 2,
                                                          statusTextWidth,
                                                          statusTextHeight), "FW:")
    cccStatusText = hs.drawing.text(hs.geometry.rect(statusText_x,
                                                     statusText_y - statusTextHeight + 1,
                                                     statusTextWidth,
                                                     statusTextHeight), "CCC:")
    arqStatusText = hs.drawing.text(hs.geometry.rect(statusText_x + 4,
                                                     statusText_y,
                                                     statusTextWidth,
                                                     statusTextHeight), "Arq:")

    firewallStatusDot = hs.drawing.circle(hs.geometry.rect(statusDot_x,
                                                           statusDot_y - (statusTextHeight*2) + 4,
                                                           statusDotWidth,
                                                           statusDotWidth))
    cccStatusDot = hs.drawing.circle(hs.geometry.rect(statusDot_x,
                                                      statusDot_y - statusTextHeight + 3,
                                                      statusDotWidth,
                                                      statusDotWidth))
    arqStatusDot = hs.drawing.circle(hs.geometry.rect(statusDot_x,
                                                      statusDot_y + 2,
                                                      statusDotWidth,
                                                      statusDotWidth))

    -- Finally, configure the rendering style of the text/circle objects, clamp them to the desktop, and show them
    firewallStatusText:setBehaviorByLabels({"canJoinAllSpaces", "stationary"}):setTextSize(11):sendToBack():show(0.5)
    cccStatusText:setBehaviorByLabels({"canJoinAllSpaces", "stationary"}):setTextSize(11):sendToBack():show(0.5)
    arqStatusText:setBehaviorByLabels({"canJoinAllSpaces", "stationary"}):setTextSize(11):sendToBack():show(0.5)

    firewallStatusDot:setBehaviorByLabels({"canJoinAllSpaces", "stationary"}):setFillColor(hs.drawing.color.osx_yellow):setStroke(false):sendToBack():show(0.5)
    cccStatusDot:setBehaviorByLabels({"canJoinAllSpaces", "stationary"}):setFillColor(hs.drawing.color.osx_yellow):setStroke(false):sendToBack():show(0.5)
    arqStatusDot:setBehaviorByLabels({"canJoinAllSpaces", "stationary"}):setFillColor(hs.drawing.color.osx_yellow):setStroke(false):sendToBack():show(0.5)
end

renderStatuslets()
