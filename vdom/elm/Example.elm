module Example exposing (all, longText, testPattern)

import Color
import ESP32 exposing (ESP32, VideoConstants)
import Node exposing (Node, Type(..))


all : List ( String, ESP32 -> VideoConstants -> Node )
all =
    [ ( "Long text", longText )
    , ( "Test pattern", testPattern )
    ]


longText : ESP32 -> VideoConstants -> Node
longText esp32 c =
    Node 0 <|
        Node.Text
            { x = c.xMin + 4
            , y = c.yMin + 2
            , text = """ !"#$%&'()*+,-./0123456789:;<=>?@
ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`
abcdefghijklmnopqrstuvwxyz{|}~

def greet(name):
    print(f"Hello, {name}!")
greet("world")"

int main() {
    cout << "OHAI" << endl;
    return 0;
}

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      (model + 1, Cmd.none)"""
            , fontIndex = 1
            , color = Color.white
            }


testPattern : ESP32 -> VideoConstants -> Node
testPattern esp32 c =
    Node.group 0
        [ -- full screen border
          Node.Rect { x = c.xMin, y = c.yMin, w = c.usableW, h = c.usableH, color = Color.white }
        , -- filled rectangle in a corner
          Node.RectFill { x = c.xMax - 20, y = c.yMax - 20, w = 16, h = 16, color = Color.gray }
        , -- center cross
          Node.XLine { x = c.xMin + 1, y = c.yCenter, len = c.usableW - 2, color = Color.gray }
        , Node.YLine { x = c.xCenter, y = c.yMin + 1, len = c.usableH - 2, color = Color.gray }
        , -- text at the top
          Node.Text { x = c.xMin + 4, y = c.yMin + 2, text = "Ping!", fontIndex = 1, color = Color.white }
        , Node.Group
            { children =
                [ -- text below it
                  Node 0 <| Node.Text { x = c.xMin + 40, y = c.yMin + 20, text = "Pong!", fontIndex = 0, color = Color.gray }
                ]
            }
        ]
