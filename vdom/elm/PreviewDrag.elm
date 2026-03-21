module PreviewDrag exposing (clampedNodeXYFromClientDrag)

import ESP32 exposing (VideoConstants)


{-| Map a client-space drag (from `mousedown` / `mousemove`) into new node `(x, y)`
in video coordinates, clamped like the Details sliders (`xMin`..`xMax`, `yMin`..`yMax`).
-}
clampedNodeXYFromClientDrag :
    VideoConstants
    -> Int
    -> ( Float, Float )
    -> ( Float, Float )
    -> ( Int, Int )
    -> ( Int, Int )
clampedNodeXYFromClientDrag vc zoom ( cx0, cy0 ) ( cx1, cy1 ) ( nx, ny ) =
    let
        dx =
            truncate ((cx1 - cx0) / toFloat zoom)

        dy =
            truncate ((cy1 - cy0) / toFloat zoom)
    in
    ( clamp vc.xMin vc.xMax (nx + dx)
    , clamp vc.yMin vc.yMax (ny + dy)
    )
