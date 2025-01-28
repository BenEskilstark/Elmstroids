module Main exposing (..)

import UI

import Browser
import Browser.Events
import Browser.Dom exposing (Viewport)
import Html exposing (Html, div, text, span)
import Html.Attributes exposing (style)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode
import List exposing (map, head, filter, indexedMap, foldl, length)
import Maybe exposing (Maybe, andThen)
import Debug exposing (toString)
import Time
import Task
import Random exposing (generate)
import Json.Decode exposing (Decoder)


main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = subs }

screenWidth : number
screenWidth = 1400
screenHeight : number
screenHeight = 1200


type alias Model = {
        tick: Int, paused: Bool,
        isDebugMode: Bool,
        entities: List Entity,
        nextId: EntityId,
        width: Int, height: Int,
        windowWidth: Int, windowHeight: Int,
        leftPressed: Bool, rightPressed: Bool, upPressed: Bool
    }
type Msg = Tick Time.Posix 
    | TogglePause | Restart
    | KeyDown String | KeyUp String 
    | WindowResize Int Int | GetViewport Viewport
    | RandomAsteroid XYSpeedTheta
    | RandomFuelDepot XYSpeedTheta
    | RandomAmmoDepot XYSpeedTheta

type alias EntityId = Int
type alias Entity = { 
    id: EntityId, name: String,
    width: Float, height: Float,
    x: Float, y: Float, 
    speed: Float, maxSpeed: Float,
    accel: Float,
    theta: Float, thetaSpeed: Float,
    ticksLeft: Int,

    fuel: Int, maxFuel: Int,
    ammo: Int, maxAmmo: Int,
    supplyRadius: Float,

    isFueled: Bool,
    isFuelDepot: Bool,
    isArmed: Bool,
    isAmmoDepot: Bool,
    isCollided: String, -- name of entity it collided with
    isDestroyed: Bool,
    isPlayerControlled: Bool,
    isShortLived: Bool
    }

type alias ModelSystem = Model -> Model
type alias EntitySystem = List Entity -> List Entity

type alias XYSpeedTheta = {
    x: Float, y: Float, speed: Float, theta: Float
    }



--------------------------------------------------------------------------------
---------------------------- INIT ----------------------------------------------
init : () -> (Model, Cmd Msg)
init _ = ({
        tick = 0, paused = False, isDebugMode = False,
        entities = [], nextId = 1, 
        width = screenWidth, height = screenHeight,
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
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomAsteroid randomXYSpeedTheta,
        generate RandomFuelDepot randomXYSpeedTheta,
        generate RandomAmmoDepot randomXYSpeedTheta
        ])

initialEntities : List Entity
initialEntities = [
    makeShip (screenWidth / 2) (screenHeight / 2)
    ]

randomXYSpeedTheta :  Random.Generator XYSpeedTheta
randomXYSpeedTheta = Random.map4 (\x y speed theta -> XYSpeedTheta x y speed theta) 
    (Random.float 0 screenWidth) 
    (Random.float 0 screenHeight) 
    (Random.float 0 2) 
    (Random.float 0 (2 * pi))



--------------------------------------------------------------------------------
--------------------------- UPDATE ---------------------------------------------

subs : Model -> Sub Msg
subs {paused} = 
    Sub.batch [
        Browser.Events.onKeyDown (Decode.field "key" Decode.string |> Decode.map KeyDown),
        Browser.Events.onKeyUp (Decode.field "key" Decode.string |> Decode.map KeyUp),
        if not paused then Time.every 30 Tick else Sub.none,
        Browser.Events.onResize WindowResize
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({tick, paused, entities} as model) = case msg of
    Tick _ -> ({ model | tick = tick + 1} |> applySystems, Cmd.none)
    TogglePause -> ({ model | paused = not paused }, Cmd.none)
    Restart -> init ()
    WindowResize w h -> ({model | windowWidth = w, windowHeight = h}, Cmd.none)
    RandomAsteroid {x, y, speed, theta} -> (addEntity (makeAsteroid x y speed theta) model, Cmd.none)
    RandomFuelDepot {x, y, speed, theta} -> (addEntity (makeFuelDepot x y (speed / 4) theta) model, Cmd.none)
    RandomAmmoDepot {x, y, speed, theta} -> (addEntity (makeAmmoDepot x y (speed / 4) theta) model, Cmd.none)
    GetViewport v -> ({model | 
        windowWidth = (round v.viewport.width), 
        windowHeight = (round v.viewport.height)
        }, Cmd.none)
    KeyDown key -> if paused then (model, Cmd.none) else case key of
        "ArrowUp" -> ({model | upPressed = True}, Cmd.none)
        "ArrowLeft" -> ({model | leftPressed = True}, Cmd.none)
        "ArrowRight" -> ({model | rightPressed = True}, Cmd.none)
        " " -> (filter .isPlayerControlled entities
                |> foldl (\ {x, y, theta, ammo} mod ->
                    if ammo > 0 then addEntity (makeLaser x y theta) mod else mod) model
                |> (\ mod ->
                    {mod | entities = map (\ e ->
                        if e.isPlayerControlled then {e | ammo = max 0 (e.ammo - 1)} else e ) 
                        mod.entities
                    }),
            Cmd.none)
        _ -> (model, Cmd.none)   
    KeyUp key -> case key of
        "ArrowUp" -> ({model | upPressed = False}, Cmd.none)
        "ArrowLeft" -> ({model | leftPressed = False}, Cmd.none)
        "ArrowRight" -> ({model | rightPressed = False}, Cmd.none)
        _ -> (model, Cmd.none)



--------------------------------------------------------------------------------
--------------------------- VIEW -----------------------------------------------
view : Model -> Html Msg
view model = if model.isDebugMode 
    then viewDebug model 
    else viewGame model

viewGame : Model -> Html Msg
viewGame ({entities, paused} as model) = UI.fullscreen
    <| div [
        preventDefaultOn "keydown" keyDecoder
    ] [
        (div [
            style "height" "100vh",
            style "position" "relative",
            style "background-color" "black"
        ] (renderEntities entities model)),
        (div [
            style "position" "absolute",
            style "display" "flex",
            style "gap" "15px",
            style "z-index" "3",
            style "background-color" "white",
            style "top" "0px",
            style "padding" "10px"
        ] [
            UI.toButton (UI.Clickable 
                (if paused then "Start" else "Pause") 
                TogglePause UI.Default
            ),
            UI.toButton (UI.Clickable "Restart" Restart UI.Reject),
            span [] [text ("Fuel: " ++ (toString ((filter .isPlayerControlled entities) |> map (\ e -> e.fuel))))],
            span [] [text ("Ammo: " ++ (toString ((filter .isPlayerControlled entities) |> map (\ e -> e.ammo))))]
        ])
    ]

keyDecoder : Decoder (Msg, Bool)
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen (\_ -> Decode.succeed (KeyDown "none", True))
    
    

viewDebug : Model -> Html Msg
viewDebug model = UI.fullscreen
    <| UI.centered
    <| UI.column [
        UI.card (div [] (
            map (\e -> div [] [text (toString e)]) model.entities
        ))
    ]



--------------------------------------------------------------------------------
--------------------------- CANVAS ---------------------------------------------

renderEntities : List Entity -> Model -> List (Html Msg)
renderEntities entities model = case entities of 
    [] -> []
    e :: es -> renderEntity e model :: renderEntities es model

renderEntity : Entity -> Model -> Html Msg
renderEntity entity model = case entity.name of 
    "Asteroid" -> render model renderAsteroid entity
    "Ship" -> render model renderShip entity
    "Laser" -> render model renderLaser entity
    "Explosion" -> render model renderExplosion entity
    "Fuel Depot" -> render model renderFuelDepot entity
    "Ammo Depot" -> render model renderAmmoDepot entity
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
            supplyRadius = mov.supplyRadius * widthRatio,
            x = mov.x * widthRatio,
            y = mov.y * heightRatio
        }

render : Model -> (Entity -> Html Msg) -> Entity -> Html Msg
render model renderFn entity = 
    let 
       ({x, y, theta, width, height} as e) = normalizeRenderingToWindow model entity
    in
        div [
            style "position" "absolute",
            style "top" ((String.fromFloat (y - height / 2)) ++ "px"), 
            style "left" ((String.fromFloat (x - width / 2)) ++ "px"),
            style "transform" ("rotate(" ++ (String.fromFloat (180 / pi * theta)) ++ "deg)")
        ] [
            -- div [style "width" ((String.fromFloat width) ++ "px"), style "height" ((String.fromFloat height) ++ "px"), style "border-radius" "50%", style "background-color" "red"] [],
            renderFn e
        ]

renderAsteroid : Entity -> Html Msg
renderAsteroid {x, y, width, height} = 
    div [
        style "width" ((String.fromFloat width) ++ "px"), 
        style "height" ((String.fromFloat height) ++ "px"),
        style "border-radius" "50%",
        style "border" "3px solid white"
    ] []

renderShip : Entity -> Html Msg
renderShip ({x, y, theta, accel, width, height}) = div [] [
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
        style "width" ((String.fromFloat width) ++ "px"), 
        style "height" ((String.fromFloat height) ++ "px"),
        style "background-color" "red"
    ] []

renderExplosion : Entity -> Html Msg
renderExplosion {x, y, ticksLeft, width} = 
    let
        radius = (round width) - ticksLeft
    in
        div [
            -- style "position" "absolute",
            -- style "top" ((String.fromFloat (y - fradius / 2)) ++ "px"), 
            -- style "left" ((String.fromFloat (x - fradius / 2)) ++ "px"),
            style "width" ((String.fromInt radius) ++ "px"), 
            style "height" ((String.fromInt radius) ++ "px"),
            style "border-radius" "50%",
            style "background-color" "orange",
            style "opacity" "50%"
        ] []

renderFuelDepot : Entity -> Html Msg
renderFuelDepot {x, y, width, height, supplyRadius, fuel, maxFuel} = 
    div [] [
        div [
            style "width" ((String.fromFloat width) ++ "px"), 
            style "height" ((String.fromFloat height) ++ "px"),
            style "background-color" "orange",
            style "font-size" "8px"
        ] [ text (toString fuel ++ "/" ++ toString maxFuel)],
        div [
            style "position" "absolute",
            style "top" ((String.fromFloat (-1 * supplyRadius + height / 2)) ++ "px"), 
            style "left" ((String.fromFloat (-1 * supplyRadius + width / 2)) ++ "px"),
            style "width" ((String.fromFloat (supplyRadius * 2)) ++ "px"), 
            style "height" ((String.fromFloat (supplyRadius * 2)) ++ "px"),
            style "border-radius" "50%",
            style "border" "1px solid red"
        ] []
    ]

renderAmmoDepot : Entity -> Html Msg
renderAmmoDepot {x, y, width, height, supplyRadius, ammo, maxAmmo} = 
    div [] [
        div [
            style "width" ((String.fromFloat width) ++ "px"), 
            style "height" ((String.fromFloat height) ++ "px"),
            style "background-color" "green",
            style "font-size" "8px"
        ] [ text (toString ammo ++ "/" ++ toString maxAmmo)],
        div [
            style "position" "absolute",
            style "top" ((String.fromFloat (-1 * supplyRadius + height / 2)) ++ "px"), 
            style "left" ((String.fromFloat (-1 * supplyRadius + width / 2)) ++ "px"),
            style "width" ((String.fromFloat (supplyRadius * 2)) ++ "px"), 
            style "height" ((String.fromFloat (supplyRadius * 2)) ++ "px"),
            style "border-radius" "50%",
            style "border" "1px solid red"
        ] []
    ]


emptyShape : Html Msg
emptyShape = span [] []



--------------------------------------------------------------------------------
-------------------------- ENTITIES --------------------------------------------
defaultEntity : Entity
defaultEntity = {
    id = 0, name = "",
    x = 0, y = 0, 
    speed = 0, maxSpeed = 0,
    theta = 0, thetaSpeed = 0,
    accel = 0, 
    width = 0, height = 0,
    ticksLeft = 0,

    fuel = 0, maxFuel = 0,
    ammo = 0, maxAmmo = 0,
    supplyRadius = 0,

    isCollided = "",
    isPlayerControlled = False,
    isShortLived = False,
    isDestroyed = False,
    isFueled = False,
    isFuelDepot = False,
    isArmed = False,
    isAmmoDepot = False
    }

addEntity : Entity -> Model -> Model
addEntity entity ({entities, nextId} as model) = {model |
    nextId = nextId + 1,
    entities = {entity | id = nextId} :: entities
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
    fuel = 300, maxFuel = 300,
    ammo = 5, maxAmmo = 10,
    isPlayerControlled = True,
    isFueled = True,
    isArmed = True
    }

makeAsteroid : Float -> Float -> Float -> Float -> Entity 
makeAsteroid x y speed theta = {defaultEntity | 
    x = x, y = y, width = 20, height = 20,
    name = "Asteroid",
    speed = speed,
    theta = theta,
    maxSpeed = 2
    }

makeLaser : Float -> Float -> Float -> Entity
makeLaser x y theta = {defaultEntity |
    x = x, y = (y), width = 15, height = 2, 
    name = "Laser",
    speed = 8,
    theta = theta,
    maxSpeed = 8,
    ticksLeft = 50,
    isShortLived = True
    }

makeExplosion : Float -> Float -> Int -> Entity
makeExplosion x y ticks = {defaultEntity | 
        name = "Explosion",
        x = x, y = y,
        width = 20,
        ticksLeft = ticks,
        isShortLived = True
    }

makeFuelDepot : Float -> Float -> Float -> Float -> Entity
makeFuelDepot x y speed theta = {defaultEntity |
    name = "Fuel Depot",
    x = x, y = y, 
    width = 20, height = 20,
    supplyRadius = 150,
    speed = speed,
    theta = theta,
    maxSpeed = 1,
    fuel = 1000, maxFuel = 1000,
    isFuelDepot = True
    }

makeAmmoDepot : Float -> Float -> Float -> Float -> Entity
makeAmmoDepot x y speed theta = {defaultEntity |
    name = "Ammo Depot",
    x = x, y = y, 
    width = 20, height = 20,
    supplyRadius = 120,
    speed = speed,
    theta = theta,
    maxSpeed = 1,
    ammo = 50, maxAmmo = 50,
    isAmmoDepot = True
    }



--------------------------------------------------------------------------------
--------------------------- SYSTEMS --------------------------------------------
applySystems : ModelSystem
applySystems model = 
    {model | entities = moveEntities model.entities}
    |> wrapEntities
    |> (\ mod -> {mod | entities = tickEntities mod.entities})
    |> commandPlayerEntities
    |> (\ mod -> {mod | entities = refuelEntities mod.entities})
    |> (\ mod -> {mod | entities = rearmEntities mod.entities})
    |> (\ mod -> {mod | entities = useFuel mod.entities})
    |> (\ mod -> {mod | entities = computeCollisions mod.entities})
    |> (\ mod -> {mod | entities = destroyCollisions mod.entities})
    |> makeExplosions
    |> (\ mod -> {mod | entities = destroyEntities mod.entities})


moveEntities : EntitySystem
moveEntities entities = map moveEntity entities

moveEntity : Entity -> Entity
moveEntity ({x, y, speed, maxSpeed, accel, theta, thetaSpeed} as entity) =
    {entity | 
        x = x + (speed * cos theta), y = y + (speed * sin theta),
        speed = min (speed + accel) maxSpeed,
        theta = clampTheta (theta + thetaSpeed)
        }


wrapEntities : ModelSystem
wrapEntities ({width, height, entities} as model)  = { model | entities = 
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
    }


tickEntities : EntitySystem
tickEntities entities = map (\e -> if e.isShortLived == True
        then {e | 
            ticksLeft = e.ticksLeft - 1, 
            isDestroyed = e.ticksLeft < 0
        } else e
    ) entities


commandPlayerEntities : ModelSystem
commandPlayerEntities ({leftPressed, rightPressed, upPressed, entities} as model)  =
    {model | entities = 
        map (\ e -> if e.isPlayerControlled 
            then {e |
                thetaSpeed = if leftPressed then -0.1 else if rightPressed then 0.1 else 0,
                accel = if upPressed && e.fuel > 0 then 0.5 else if e.speed > 0 then -0.1 else 0
            } else e
        ) entities
    }


refuelEntities : EntitySystem
refuelEntities entities = foldl
    (resupplyEntity .isFuelDepot fuelAccessor 2)
    entities 
    (List.filter .isFueled entities)

resupplyEntity : (Entity -> Bool) -> FieldAccessor Int -> Int -> Entity -> List Entity -> List Entity
resupplyEntity fn accessor amount e entities =
    let 
        depots = List.filter fn entities
    in 
        case inAnyRadius e depots of
            Nothing -> entities
            Just depotId -> case getEntity depotId entities of
                Nothing -> entities -- shouldn't happen
                Just depot ->
                    let
                        (updatedDepot, updatedEntity) = transferVal accessor amount depot e
                    in
                        setEntity updatedEntity (setEntity updatedDepot entities)

transferVal : FieldAccessor Int -> Int -> Entity -> Entity -> (Entity, Entity)
transferVal accessor amount fromEntity toEntity =
    let
        currentAmountTo = accessor.getter toEntity
        maxCapacityTo = accessor.maxGetter toEntity
        spaceAvailable = maxCapacityTo - currentAmountTo
        amountToTransfer = min amount spaceAvailable
        { result, amountSubtracted } = trySubtract (accessor.getter fromEntity) amountToTransfer
        newFrom = accessor.setter result fromEntity
        newTo = accessor.setter (currentAmountTo + amountSubtracted) toEntity
    in
    (newFrom, newTo)

inAnyRadius : Entity -> List Entity -> Maybe EntityId 
inAnyRadius entity depots = filter (inRadius entity) depots
    |> head |> andThen (\e -> Just e.id)

inRadius : Entity -> Entity -> Bool
inRadius ent {x, y, supplyRadius} = 
    sqrt ((ent.x - x) ^ 2 + (ent.y - y) ^ 2) <= supplyRadius


rearmEntities : EntitySystem
rearmEntities entities = foldl 
    (resupplyEntity .isAmmoDepot ammoAccessor 1)
    entities 
    (List.filter .isArmed entities)


useFuel : EntitySystem
useFuel entities = 
    map (\ e -> if e.isFueled 
        then {e | 
            fuel = if e.accel > 0 then max 0 (e.fuel - 1) else e.fuel,
            accel = if e.accel > 0 && e.fuel <= 0 then 0 else e.accel
        } 
        else e
    ) entities


computeCollisions : EntitySystem
computeCollisions entities = case entities of
    [] -> []
    ea :: rest -> case collidesAny ea rest of
        Nothing -> ea :: computeCollisions rest
        Just eb -> {ea | isCollided = eb.name} 
            :: computeCollisions (setEntity eb rest)

collidesAny : Entity -> List Entity -> Maybe Entity
collidesAny entity entities = case entities of
    [] -> Nothing
    e :: rest -> if collides entity e 
        then Just { e | isCollided = entity.name }
        else collidesAny entity rest

collides : Entity -> Entity -> Bool
collides ea eb = ((eb.x > ea.x && eb.x < ea.x + ea.width) ||
    (ea.x > eb.x && ea.x < eb.x + eb.width)) &&
    ((eb.y > ea.y && eb.y < ea.y + ea.height) ||
    (ea.y > eb.y && ea.y < eb.y + eb.height))


destroyCollisions : EntitySystem
destroyCollisions entities = map (\ e -> 
    case e.isCollided of
        "Asteroid" -> if e.name /= "Asteroid" then {e |isDestroyed = True} else e
        "Laser" -> if e.name /= "Ship" then {e |isDestroyed = True} else e
        "Ship" -> if e.name /= "Laser" then {e |isDestroyed = True} else e
        "Explosion" -> e
        "" -> e
        _ -> {e | isDestroyed = True}
    ) entities


makeExplosions : ModelSystem
makeExplosions ({nextId, entities} as model) =
    let
        explosions = filter 
            (\e -> e.name /= "Explosion" && e.name /= "Laser" ) entities
            |> filter .isDestroyed
            |> indexedMap (\ i {x, y} -> (makeExplosion x y 20) |> (\ e -> {e | id = nextId + i}))
    in { model | 
        nextId = length explosions + nextId, 
        entities = explosions ++ entities
    }


destroyEntities : EntitySystem
destroyEntities entities = filter (\e -> e.isDestroyed == False) entities



--------------------------------------------------------------------------------
--------------------------- HELPERS --------------------------------------------

type alias FieldAccessor a = {
        getter : Entity -> a,
        maxGetter : Entity -> a,
        setter : a -> Entity -> Entity
    }

ammoAccessor : FieldAccessor Int
ammoAccessor = {
    getter = .ammo,
    maxGetter = .maxAmmo,
    setter = (\ value entity -> { entity | ammo = value })
    }

fuelAccessor : FieldAccessor Int
fuelAccessor = {
    getter = .fuel,
    maxGetter = .maxFuel,
    setter = (\ value entity -> { entity | fuel = value })
    }


trySubtract : number -> number -> {result: number, amountSubtracted: number}
trySubtract subFrom sub = if subFrom - sub < 0
    then {result = 0, amountSubtracted = subFrom}
    else {result = subFrom - sub, amountSubtracted = sub}

clampTheta : Float -> Float
clampTheta theta = if theta > 2 * pi 
    then theta - 2 * pi
    else if theta < -2 * pi
    then theta + 2 * pi
    else theta

getEntity : EntityId -> List Entity -> Maybe Entity
getEntity id entities = case entities of 
    [] -> Nothing
    e :: rest -> if e.id == id then Just e else getEntity id rest

setEntity : Entity -> List Entity -> List Entity
setEntity entity entities = case entities of 
    [] -> []
    e :: rest -> if e.id == entity.id then entity :: rest else
        e :: setEntity entity rest