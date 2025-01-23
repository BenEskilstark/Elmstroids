module Main exposing (..)

import UI

import Browser
import Browser.Events
import Browser.Dom exposing (Viewport)
import Html exposing (Html, div, text, span)
import Html.Attributes exposing (style)
import List exposing (map, filter, head)
import Maybe exposing (Maybe, withDefault)
import Debug exposing (toString)
import Time
import Task
import Json.Decode
import Random exposing (generate)


main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = subs }

type alias Model = {
        tick: Int, paused: Bool,
        isDebugMode: Bool,
        entities: List Entity,
        nextId: Int,
        width: Int, height: Int,
        windowWidth: Int, windowHeight: Int,
        leftPressed: Bool, rightPressed: Bool, upPressed: Bool
    }
type Msg = Tick Time.Posix | TogglePause 
    | Keydown String | Keyup String 
    | WindowResize Int Int | GetViewport Viewport
    | RandomAsteroid XYSpeedTheta

type alias Entity = { 
    id: Int, name: String,
    width: Maybe Float, height: Maybe Float,
    x: Maybe Float, y: Maybe Float, 
    speed: Maybe Float, maxSpeed: Maybe Float,
    accel: Maybe Float,
    theta: Maybe Float, thetaSpeed: Maybe Float,
    isPlayerControlled: Maybe Bool
    }
type alias System model = model -> List Entity -> List Entity
type alias XYSpeedTheta = {
    x: Float, y: Float, speed: Float, theta: Float
    }

-----------------------------------------------------------------
---------------------------- INIT -------------------------------
init : () -> (Model, Cmd Msg)
init _ = ({
        tick = 0, paused = False, isDebugMode = False,
        entities = [], nextId = 1, 
        width = 1400, height = 800,
        windowWidth = 600, windowHeight = 600,
        leftPressed = False, rightPressed = False, upPressed = False
        } 
    |> addEntities initialEntities, 
    Cmd.batch [
        Task.perform GetViewport Browser.Dom.getViewport,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta
        ])

initialEntities : List Entity
initialEntities = [
    makeShip 250 250
    ]

randomXYSpeedTheta :  Random.Generator XYSpeedTheta
randomXYSpeedTheta = Random.map4 (\x y speed theta -> XYSpeedTheta x y speed theta) 
    (Random.float 0 1400) 
    (Random.float 0 800) 
    (Random.float 0 2) 
    (Random.float 0 (2 * pi))

-----------------------------------------------------------------
--------------------------- UPDATE ------------------------------

subs : Model -> Sub Msg
subs {paused} = 
    Sub.batch [
        Browser.Events.onKeyDown (Json.Decode.field "key" Json.Decode.string |> Json.Decode.map Keydown),
        Browser.Events.onKeyUp (Json.Decode.field "key" Json.Decode.string |> Json.Decode.map Keyup),
        if not paused then Time.every 30 Tick else Sub.none,
        Browser.Events.onResize WindowResize
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({tick, paused, entities} as model) = case msg of
    Tick _ -> ({ model | tick = tick + 1, entities = applySystems model entities }, Cmd.none)
    TogglePause -> ({ model | paused = not paused }, Cmd.none)
    WindowResize w h -> ({model | windowWidth = w, windowHeight = h}, Cmd.none)
    RandomAsteroid {x, y, speed, theta} -> (addEntity (makeAsteroid x y speed theta) model, Cmd.none)
    GetViewport v -> ({model | 
        windowWidth = (round v.viewport.width), 
        windowHeight = (round v.viewport.height)
        }, Cmd.none)
    Keydown key -> case key of
        "ArrowUp" -> ({model | upPressed = True}, Cmd.none)
        "ArrowLeft" -> ({model | leftPressed = True}, Cmd.none)
        "ArrowRight" -> ({model | rightPressed = True}, Cmd.none)
        " " -> (addEntity (makeLaser model) model, Cmd.none)
        _ -> (model, Cmd.none)   
    Keyup key -> case key of
        "ArrowUp" -> ({model | upPressed = False}, Cmd.none)
        "ArrowLeft" -> ({model | leftPressed = False}, Cmd.none)
        "ArrowRight" -> ({model | rightPressed = False}, Cmd.none)
        _ -> (model, Cmd.none)



-----------------------------------------------------------------
--------------------------- VIEW --------------------------------
view : Model -> Html Msg
view model = if model.isDebugMode 
    then viewDebug model 
    else viewGame model

viewGame : Model -> Html Msg
viewGame ({entities} as model) = UI.fullscreen
    <| div [
            style "height" "100vh",
            style "position" "relative",
            style "background-color" "black"
        ] (renderEntities entities model)

viewDebug : Model -> Html Msg
viewDebug model = UI.fullscreen
    <| UI.centered
    <| UI.column [
        UI.card (div [] (
            map (\e -> div [] [text (toString e)]) model.entities
        ))
    ]

-----------------------------------------------------------------
--------------------------- CANVAS ------------------------------

renderEntities : List Entity -> Model -> List (Html Msg)
renderEntities entities model = case entities of 
    [] -> []
    e :: es -> renderEntity e model :: renderEntities es model

renderEntity : Entity -> Model -> Html Msg
renderEntity entity model = case entity.name of 
    "Asteroid" -> case toMoveable entity of
        Nothing -> emptyShape
        Just asteroid -> renderAsteroid (normalizeRenderingToWindow model asteroid)
    "Ship" -> case toMoveable entity of
        Nothing -> emptyShape
        Just ship -> renderShip (normalizeRenderingToWindow model ship)
    "Laser" -> case toMoveable entity of
        Nothing -> emptyShape
        Just laser -> renderLaser (normalizeRenderingToWindow model laser)
    _ -> emptyShape

normalizeRenderingToWindow : Model -> Moveable -> Moveable
normalizeRenderingToWindow {width, height, windowWidth, windowHeight} mov =
    let 
        widthRatio = (toFloat windowWidth) / (toFloat width)
        heightRatio = (toFloat windowHeight) / (toFloat height)
    in
        {mov | 
            width = mov.width * widthRatio, 
            height = mov.height * heightRatio,
            x = mov.x * widthRatio,
            y = mov.y * heightRatio
        }


renderAsteroid : Moveable -> Html Msg
renderAsteroid {x, y, width, height} = 
    div [
        style "position" "absolute",
        style "top" ((String.fromFloat y) ++ "px"), 
        style "left" ((String.fromFloat x) ++ "px"),
        style "width" ((String.fromFloat width) ++ "px"), 
        style "height" ((String.fromFloat height) ++ "px"),
        style "border-radius" "50%",
        style "border" "3px solid white"
    ] []

renderShip : Moveable -> Html Msg
renderShip ({x, y, theta, accel, width, height}) = div [
        style "position" "absolute",
        style "top" ((String.fromFloat y) ++ "px"), 
        style "left" ((String.fromFloat x) ++ "px"),
        style "transform" ("rotate(" ++ (String.fromFloat (180 / pi * theta)) ++ "deg)")
    ] [
        div [
            style "border-bottom" (toString (width / 2) ++ "px solid transparent"),
            style "border-top" (toString (width / 2) ++ "px solid transparent"),
            style "border-left" (toString height ++ "px solid steelblue")
            
        ] [],
        if accel <= 0 then emptyShape else (
            div [
                style "position" "absolute",
                style "top" ((String.fromFloat (height / 4)) ++ "px"), 
                style "left" ((String.fromFloat (-1 * width / 2)) ++ "px"),
                style "border-bottom" (toString (width / 4) ++ "px solid transparent"),
                style "border-top" (toString (width / 4) ++ "px solid transparent"),
                style "border-right" (toString (height / 2) ++ "px solid orange")
            ] []
        )
    ]

renderLaser : Moveable -> Html Msg
renderLaser {x, y, width, height, theta} = 
    div [
        style "position" "absolute",
        style "top" ((String.fromFloat y) ++ "px"), 
        style "left" ((String.fromFloat x) ++ "px"),
        style "width" ((String.fromFloat width) ++ "px"), 
        style "height" ((String.fromFloat height) ++ "px"),
        style "background-color" "red",
        style "transform" ("rotate(" ++ (String.fromFloat (180 / pi * theta)) ++ "deg)")
    ] []

emptyShape : Html Msg
emptyShape = span [] []

-----------------------------------------------------------------
-------------------------- ENTITIES -----------------------------
defaultEntity : Entity
defaultEntity = {
    id = 0, name = "",
    x = Just 0, y = Just 0, 
    speed = Just 0, maxSpeed = Just 0,
    theta = Just 0, thetaSpeed = Just 0,
    accel = Just 0, 
    width = Just 0, height = Just 0, 
    isPlayerControlled = Nothing
    }

addEntity : Entity -> Model -> Model
addEntity entity ({entities, nextId} as model) = {model |
    nextId = nextId + 1,
    entities = entity :: entities
    }
addEntities : List Entity -> Model -> Model
addEntities entities model = case entities of 
    [] -> model
    e :: es -> addEntity e model |> addEntities es

makeShip : Float -> Float -> Entity 
makeShip x y = makeMoveable x y 15 15 
    |> (\m -> {m | 
        name = "Ship",
        maxSpeed = Just 4,
        isPlayerControlled = Just True
        })

makeAsteroid : Float -> Float -> Float -> Float -> Entity 
makeAsteroid x y speed theta = makeMoveable x y 20 20 
    |>  (\m -> {m |
        name = "Asteroid",
        speed = Just speed,
        theta = Just theta,
        maxSpeed = Just 2
        })

makeLaser : Model -> Entity
makeLaser {entities} = filter (\e -> e.isPlayerControlled == Just True) entities
    |> head |> withDefault defaultEntity
    |> toMoveable |> withDefault defaultMoveable
    |> (\ {x, y, theta} -> 
        makeMoveable x (y + 6) 15 2 
        |>  (\m -> {m |
            name = "Laser",
            speed = Just 8,
            theta = Just theta,
            maxSpeed = Just 8
            })
    )


type alias Moveable = {
    x: Float, y: Float, speed: Float, maxSpeed: Float, accel: Float, 
    theta: Float, thetaSpeed: Float, 
    width: Float, height: Float
    }
defaultMoveable : Moveable
defaultMoveable = {
    x = 0, y = 0, speed = 0, maxSpeed = 0, accel = 0,
    theta = 0, thetaSpeed = 0, width = 0, height = 0
    }

toMoveable : Entity -> Maybe Moveable
toMoveable {x, y, speed, maxSpeed, accel, theta, thetaSpeed, width, height} = 
    Just Moveable
        |> andMap x
        |> andMap y
        |> andMap speed
        |> andMap maxSpeed
        |> andMap accel
        |> andMap theta
        |> andMap thetaSpeed
        |> andMap width
        |> andMap height

makeMoveable : Float -> Float -> Float -> Float ->  Entity
makeMoveable x y width height = {defaultEntity |
    x = Just x, y = Just y,
    width = Just width, height = Just height
    }


type alias Playable = {
    speed: Float, accel: Float, 
    thetaSpeed: Float, isPlayerControlled: Bool
    }

toPlayable : Entity -> Maybe Playable
toPlayable {thetaSpeed, accel, isPlayerControlled, speed} = 
    Just Playable
        |> andMap speed
        |> andMap accel
        |> andMap thetaSpeed
        |> andMap isPlayerControlled
        


andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap = Maybe.map2 (|>)


-----------------------------------------------------------------
--------------------------- SYSTEMS -----------------------------
applySystems : System Model
applySystems model entities = 
    moveEntities model entities
    |> commandPlayerEntities model


moveEntities : System Model
moveEntities _ entities = map moveEntity entities

moveEntity : Entity -> Entity
moveEntity entity = case (toMoveable entity) of
    Nothing -> entity
    Just ({x, y, speed, maxSpeed, accel, theta, thetaSpeed}) -> {entity | 
        x = Just (x + (speed * cos theta)), y = Just (y + (speed * sin theta)),
        speed = Just (min (speed + accel) maxSpeed),
        theta = Just (clampTheta (theta + thetaSpeed))
        }

commandPlayerEntities : System Model
commandPlayerEntities {leftPressed, rightPressed, upPressed} entities =
    map (\ e -> case toPlayable e of
        Nothing -> e
        Just ({speed}) -> {e |
                thetaSpeed = (if leftPressed then Just -0.1 else if rightPressed then Just 0.1 else Just 0),
                accel = if upPressed then Just 0.5 else if speed > 0 then Just -0.1 else Just 0
            }
    
    ) entities

clampTheta : Float -> Float
clampTheta theta = if theta > 2 * pi 
    then theta - 2 * pi
    else if theta < -2 * pi
    then theta + 2 * pi
    else theta