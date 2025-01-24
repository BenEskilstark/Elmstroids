module Main exposing (..)

import UI

import Browser
import Browser.Events
import Browser.Dom exposing (Viewport)
import Html exposing (Html, div, text, span)
import Html.Attributes exposing (style)
import List exposing (map, filter, head, append)
import Maybe exposing (Maybe, withDefault)
import Debug exposing (toString)
import Time
import Task
import Json.Decode
import Random exposing (generate)
import Color exposing (Color)


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
    width: Float, height: Float,
    x: Float, y: Float, 
    speed: Float, maxSpeed: Float,
    accel: Float,
    theta: Float, thetaSpeed: Float,
    ticksLeft: Int,

    isDestroyed: Bool,
    isPlayerControlled: Bool,
    isShortLived: Bool
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
        width = 1400, height = 1200,
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
    (Random.float 0 1200) 
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
    "Asteroid" -> renderAsteroid (normalizeRenderingToWindow model entity)
    "Ship" -> renderShip (normalizeRenderingToWindow model entity)
    "Laser" -> renderLaser (normalizeRenderingToWindow model entity)
    "Explosion" -> renderExplosion (normalizeRenderingToWindow model entity)
    _ -> emptyShape

normalizeRenderingToWindow : Model -> Entity -> Entity
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

renderAsteroid : Entity -> Html Msg
renderAsteroid {x, y, width, height} = 
    div [
        style "position" "absolute",
        style "top" ((String.fromFloat y) ++ "px"), 
        style "left" ((String.fromFloat x) ++ "px"),
        style "width" ((String.fromFloat (width * 1.2)) ++ "px"), 
        style "height" ((String.fromFloat (height * 1.2)) ++ "px"),
        style "border-radius" "50%",
        style "border" "3px solid white"
    ] []

renderShip : Entity -> Html Msg
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

renderLaser : Entity -> Html Msg
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

renderExplosion : Entity -> Html Msg
renderExplosion {x, y, ticksLeft, width} = 
    let
        radius = (round width) - ticksLeft
        fradius = toFloat radius
    in
        div [
            style "position" "absolute",
            style "top" ((String.fromFloat (y - fradius / 2)) ++ "px"), 
            style "left" ((String.fromFloat (x - fradius / 2)) ++ "px"),
            style "width" ((String.fromInt radius) ++ "px"), 
            style "height" ((String.fromInt radius) ++ "px"),
            style "border-radius" "50%",
            style "background-color" "orange",
            style "opacity" "50%"
        ] []


emptyShape : Html Msg
emptyShape = span [] []

-----------------------------------------------------------------
-------------------------- ENTITIES -----------------------------
defaultEntity : Entity
defaultEntity = {
    id = 0, name = "",
    x = 0, y = 0, 
    speed = 0, maxSpeed = 0,
    theta = 0, thetaSpeed = 0,
    accel = 0, 
    width = 0, height = 0,
    ticksLeft = 0,
    isPlayerControlled = False,
    isShortLived = False,
    isDestroyed = False
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
makeShip x y = {defaultEntity | 
    x = x, y = y, width = 15, height = 15,
    name = "Ship",
    maxSpeed = 4,
    isPlayerControlled = True
    }

makeAsteroid : Float -> Float -> Float -> Float -> Entity 
makeAsteroid x y speed theta = {defaultEntity | 
    x = x, y = y, width = 20, height = 20,
    name = "Asteroid",
    speed = speed,
    theta = theta,
    maxSpeed = 2
    }

makeLaser : Model -> Entity
makeLaser {entities} = filter .isPlayerControlled entities
    |> head |> withDefault defaultEntity
    |> (\ {x, y, theta} -> {defaultEntity |
        x = x, y = (y + 6), width = 15, height = 2, 
        name = "Laser",
        speed = 8,
        theta = theta,
        maxSpeed = 8,
        ticksLeft = 50,
        isShortLived = True
        }
    )

makeExplosion : Float -> Float -> Int -> Entity
makeExplosion x y ticks = {defaultEntity | 
        name = "Explosion",
        x = x, y = y,
        width = 20,
        ticksLeft = ticks,
        isShortLived = True
    }


-----------------------------------------------------------------
--------------------------- SYSTEMS -----------------------------
applySystems : System Model
applySystems model entities = 
    moveEntities model entities
    |> wrapEntities model
    |> tickEntities model
    |> commandPlayerEntities model
    |> computeCollisions model
    |> makeExplosions model
    |> destroyEntities model


moveEntities : System Model
moveEntities _ entities = map moveEntity entities

moveEntity : Entity -> Entity
moveEntity ({x, y, speed, maxSpeed, accel, theta, thetaSpeed} as entity) =
    {entity | 
        x = x + (speed * cos theta), y = y + (speed * sin theta),
        speed = min (speed + accel) maxSpeed,
        theta = clampTheta (theta + thetaSpeed)
        }

commandPlayerEntities : System Model
commandPlayerEntities {leftPressed, rightPressed, upPressed} entities =
    map (\ e -> if e.isPlayerControlled 
        then {e |
            thetaSpeed = if leftPressed then -0.1 else if rightPressed then 0.1 else 0,
            accel = if upPressed then 0.5 else if e.speed > 0 then -0.1 else 0
        } else e
    ) entities

wrapEntities : System Model
wrapEntities {width, height} entities = 
    map (\ ({x, y} as e) -> {e | 
        x = if x < -20 
                then x + (toFloat width) + 20 
            else if x > (toFloat width) + 20 
                then x - (toFloat width) - 20
            else x,
        y = if y < -20 
                then y + (toFloat height) + 20 
            else if y > (toFloat height) + 20 
                then y - (toFloat height) - 20
            else y
    }
    ) entities

tickEntities : System Model
tickEntities _ entities = map (\e -> if e.isShortLived == True
        then {e | 
            ticksLeft = e.ticksLeft - 1, 
            isDestroyed = e.ticksLeft < 0
        } else e
    ) entities


computeCollisions : System Model
computeCollisions model entities = case entities of
    [] -> entities
    _ :: [] -> entities
    ea :: eb :: es -> if (collides ea eb) then
        {ea | isDestroyed = True} :: computeCollisions model ({eb | isDestroyed = True} :: es)
        else 
            ea :: computeCollisions model (eb :: es)

collides : Entity -> Entity -> Bool
collides ea eb = (eb.x > ea.x && eb.x < ea.x + ea.width &&
    eb.y > ea.y && eb.y < ea.y + ea.height) ||
    (ea.x > eb.x && ea.x < eb.x + eb.width &&
    ea.y > eb.y && ea.y < eb.y + eb.height) 


makeExplosions : System Model 
makeExplosions _ entities = filter (\e -> e.name /= "Explosion") entities
    |> filter .isDestroyed
    |> map (\{x, y} -> makeExplosion x y 20)
    |> append entities 

destroyEntities : System Model
destroyEntities _ entities = filter (\e -> e.isDestroyed == False) entities


clampTheta : Float -> Float
clampTheta theta = if theta > 2 * pi 
    then theta - 2 * pi
    else if theta < -2 * pi
    then theta + 2 * pi
    else theta