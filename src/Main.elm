module Main exposing (..)

import UI

import Browser
import Browser.Events
import Browser.Dom exposing (Viewport)
import Html exposing (Html, div, text, span, ul, li, h4)
import Html.Attributes exposing (style)
import Html.Events exposing (preventDefaultOn)
import Json.Decode as Decode
import List exposing (map, head, filter, indexedMap, foldl, length, any)
import Maybe exposing (Maybe, andThen)
import Debug exposing (toString)
import Time
import Task
import Random exposing (generate)
import Random.Extra exposing (choices)
import Json.Decode exposing (Decoder)


main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = subs }

screenWidth : number
screenWidth = 1300
-- screenWidth = 1470
screenHeight : number
screenHeight = 800
-- screenHeight = 832


type alias Model = {
        tick: Int, paused: Bool,
        isDebugMode: Bool,
        entities: List Entity,
        nextId: EntityId,
        width: Int, height: Int,
        windowWidth: Int, windowHeight: Int,
        leftPressed: Bool, rightPressed: Bool, upPressed: Bool,
        isGameLost: Bool, isGameWon: Bool,
        difficulty: Int
    }
type Msg = Tick Time.Posix 
    | TogglePause | Restart Int
    | KeyDown String | KeyUp String 
    | WindowResize Int Int | GetViewport Viewport
    | RandomAsteroid XYSpeedTheta
    | RandomBigAsteroid XYSpeedTheta
    | RandomFuelDepot XYSpeedTheta
    | RandomAmmoDepot XYSpeedTheta

type alias EntityId = Int
type alias Entity = { 
    id: EntityId, name: String,
    radius: Float,
    x: Float, y: Float, 
    speed: Float, maxSpeed: Float,
    accel: Float,
    theta: Float, thetaSpeed: Float,

    isCollided: Bool, 
    collidedWith: String, -- name of entity it collided with

    isDestroyed: Bool,
    isPlayerControlled: Bool,

    isFueled: Bool,
    isFuelDepot: Bool,
    fuel: Int, maxFuel: Int,
    isArmed: Bool,
    isAmmoDepot: Bool,
    ammo: Int, maxAmmo: Int,
    supplyRadius: Float,

    isShortLived: Bool,
    ticksLeft: Int,

    isShielded: Bool,
    shieldRadius: Float,
    shieldHP: Int
    }

type alias ModelSystem = Model -> Model
type alias EntitySystem = List Entity -> List Entity

type alias XYSpeedTheta = {
    x: Float, y: Float, speed: Float, theta: Float
    }



--------------------------------------------------------------------------------
---------------------------- INIT ----------------------------------------------
init : () -> (Model, Cmd Msg)
init _ = doInit 2

doInit : Int -> (Model, Cmd Msg)
doInit difficulty = ({
        tick = 0, paused = True, isDebugMode = False,
        entities = [], nextId = 1, 
        width = screenWidth, height = screenHeight,
        windowWidth = 600, windowHeight = 600,
        leftPressed = False, rightPressed = False, upPressed = False,
        isGameLost = False, isGameWon = False, 
        difficulty = difficulty
        } 
    |> addEntities initialEntities, 
    Cmd.batch ([
        Task.perform GetViewport Browser.Dom.getViewport,
        generate RandomFuelDepot (innerRandomXYSpeedTheta 0.25),
        generate RandomAmmoDepot (innerRandomXYSpeedTheta 0.25)
        ]
        ++ ((generateAsteroids (8 + 2 * difficulty) (1.8 + 0.1 * (toFloat difficulty)))
        ++ (generateBigAsteroids (2 * difficulty) 1.5))
    ))

generateAsteroids : number -> Float -> List (Cmd Msg)
generateAsteroids numAsteroids speed = if numAsteroids == 0 then [] else
    generate RandomAsteroid (outerRandomXYSpeedTheta speed) 
        :: (generateAsteroids (numAsteroids - 1) speed)

generateBigAsteroids : number -> Float -> List (Cmd Msg)
generateBigAsteroids numAsteroids speed = if numAsteroids == 0 then [] else
    generate RandomBigAsteroid (outerRandomXYSpeedTheta speed) 
        :: (generateAsteroids (numAsteroids - 1) speed)


initialEntities : List Entity
initialEntities = [
    makeShip (screenWidth / 2) (screenHeight / 2)
    ]

randomXYSpeedTheta : Float -> Random.Generator XYSpeedTheta
randomXYSpeedTheta maxSpeed = Random.map4 (\x y speed theta -> XYSpeedTheta x y speed theta) 
    (Random.float 0 screenWidth) 
    (Random.float 0 screenHeight) 
    (Random.float 0 maxSpeed) 
    (Random.float 0 (2 * pi))

outerRandomXYSpeedTheta : Float -> Random.Generator XYSpeedTheta
outerRandomXYSpeedTheta maxSpeed = Random.map4 (\x y speed theta -> XYSpeedTheta x y speed theta) 
    (randomExcludedDomain screenWidth (1/4)) 
    (randomExcludedDomain screenHeight (1/4)) 
    (Random.float 0 maxSpeed) 
    (Random.float 0 (2 * pi))  

innerRandomXYSpeedTheta : Float -> Random.Generator XYSpeedTheta
innerRandomXYSpeedTheta maxSpeed = Random.map4 (\x y speed theta -> XYSpeedTheta x y speed theta) 
    (Random.float (screenWidth / 3) (2 * screenWidth / 3)) 
    (Random.float (screenHeight / 3) (2 * screenHeight / 3)) 
    (Random.float 0 maxSpeed) 
    (Random.float 0 (2 * pi))    

randomExcludedDomain : Float -> Float -> Random.Generator Float
randomExcludedDomain domain exclusionFraction =
    let
        exclusionSize = domain * exclusionFraction / 2
        rangeA = Random.float 0 (domain / 2 - exclusionSize)
        rangeB = Random.float (domain / 2 + exclusionSize) domain
    in
    choices rangeA [rangeB]

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
    Restart d -> doInit d |> (\ (m, cmds) -> ({m | paused = False}, cmds))
    WindowResize w h -> ({model | windowWidth = w, windowHeight = h}, Cmd.none)
    RandomBigAsteroid {x, y, speed, theta} -> (addEntity (makeBigAsteroid x y speed theta) model, Cmd.none)
    RandomAsteroid {x, y, speed, theta} -> (addEntity (makeAsteroid x y (speed + 0.25) theta) model, Cmd.none)
    RandomFuelDepot {x, y, speed, theta} -> (addEntity (makeFuelDepot x y speed theta) model, Cmd.none)
    RandomAmmoDepot {x, y, speed, theta} -> (addEntity (makeAmmoDepot x y speed theta) model, Cmd.none)
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
                    if ammo > 0 then addEntity (makeLaser (x + (cos theta) * 5) (y + (sin theta) * 5) theta) mod else mod) model
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
    else if model.paused && model.tick == 0 
        then viewStart model
        else viewGame model

viewStart : Model -> Html Msg
viewStart _ = UI.fullscreen
    <| UI.centered
    <| UI.optionCard 
        [(UI.Clickable "Start" TogglePause UI.Accept)] 
        (div [] [
            h4 [] [ text "Welcome to Stroidfall"],
            ul [] [
                li [] [text "Use left/right arrow keys to turn"],
                li [] [text "Use up arrow key to move"],
                li [] [text "Press spacebar to shoot"],
                li [] [text "Replenish fuel at the orange station, ammo at the green station"],
                li [] [text "Can you protect your stations (and yourself) from the 'stroids?"]
            ]
        ])


viewGame : Model -> Html Msg
viewGame ({entities, paused, isGameLost, isGameWon, difficulty} as model) = UI.fullscreen
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
            UI.toButton (UI.Clickable "Restart" (Restart difficulty) UI.Reject),
            span [] [text ("Fuel: " ++ (toString ((filter .isPlayerControlled entities) |> map (\ e -> e.fuel))))],
            span [] [text ("Ammo: " ++ (toString ((filter .isPlayerControlled entities) |> map (\ e -> e.ammo))))],
            (if difficulty > 2 
                then span [] [text ("Level: " ++ (toString (difficulty - 1)))]
                else span [] []
            )
        ]),
        (if isGameLost then 
            UI.modal [(UI.Clickable "Restart" (Restart difficulty) UI.Reject)] 
            (div [] [
                h4 [] [ text "You've been destroyed!"],
                text "Try Again?"
            ]) else span [] []
        ),
        (if isGameWon then 
            UI.modal [(UI.Clickable "Next Difficulty" (Restart (difficulty + 1)) UI.Reject)] 
            (div [] [
                h4 [] [ text "You Won!"],
                text "Try again on higher difficulty (more, faster, asteroids)?"
            ]) else span [] []
        )
    ]

keyDecoder : Decoder (Msg, Bool)
keyDecoder = Decode.field "key" Decode.string
    |> Decode.andThen (\key -> Decode.succeed (KeyDown "none", key == " "))
    

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
    "Big Asteroid" -> render model renderAsteroid entity
    "Ship" -> render model renderShip entity
    "Laser" -> render model renderLaser entity
    "Explosion" -> render model renderExplosion entity
    "Fuel Depot" -> render model renderFuelDepot entity
    "Ammo Depot" -> render model renderAmmoDepot entity
    _ -> noShape

normalizeRenderingToWindow : Model -> Entity -> Entity
normalizeRenderingToWindow {width, height, windowWidth, windowHeight} mov = 
    let 
        widthRatio = (toFloat windowWidth) / (toFloat width)
        heightRatio = (toFloat windowHeight) / (toFloat height)
    in
        {mov | 
            radius = mov.radius * widthRatio, 
            supplyRadius = mov.supplyRadius * widthRatio,
            x = mov.x * widthRatio,
            y = mov.y * heightRatio
        }

render : Model -> (Entity -> Html Msg) -> Entity -> Html Msg
render model renderFn entity = 
    let 
       ({x, y, theta, radius} as e) = normalizeRenderingToWindow model entity
    in
        div [
            style "position" "absolute",
            style "top" ((String.fromFloat (y - radius)) ++ "px"), 
            style "left" ((String.fromFloat (x - radius)) ++ "px"),
            style "transform-origin" "center",
            style "transform" ("rotate(" ++ (String.fromFloat (180 / pi * theta)) ++ "deg)")
        ] [
            renderFn e,
            renderShield e
            -- , renderBoundingCircle e
        ]

renderAsteroid : Entity -> Html Msg
renderAsteroid {radius} = 
    div [
        style "width" ((String.fromFloat (radius * 2)) ++ "px"), 
        style "height" ((String.fromFloat (radius * 2)) ++ "px"),
        style "border-radius" "50%",
        style "border" "3px solid white",
        style "box-sizing" "border-box"
    ] []

renderShip : Entity -> Html Msg
renderShip ({accel, radius}) = div [] [
    div [
        style "border-bottom" (toString radius ++ "px solid transparent"),
        style "border-top" (toString radius ++ "px solid transparent"),
        style "border-left" (toString (radius * 2) ++ "px solid steelblue")
        -- style "background-color" "steelblue",
        -- style "width" ((String.fromFloat width) ++ "px"), 
        -- style "height" ((String.fromFloat height) ++ "px")
        
    ] [],
    if accel <= 0 then noShape else (
        div [
            style "position" "absolute",
            style "top" ((String.fromFloat (radius / 2)) ++ "px"), 
            style "left" ((String.fromFloat (-1 * radius)) ++ "px"),
            style "border-bottom" (toString (radius / 2) ++ "px solid transparent"),
            style "border-top" (toString (radius / 2) ++ "px solid transparent"),
            style "border-right" (toString radius ++ "px solid orange")
        ] []
    )
    ]

renderShield : Entity -> Html Msg
renderShield {shieldHP, shieldRadius, radius} = if shieldHP <= 0 then noShape else
    div
        [ style "position" "absolute"
        , style "border" "1px solid blue"
        , style "border-radius" "50%"
        , style "width" (String.fromFloat (shieldRadius * 2) ++ "px")
        , style "height" (String.fromFloat (shieldRadius * 2) ++ "px")
        , style "top" ((String.fromFloat (-1 * shieldRadius + radius)) ++ "px")
        , style "left" ((String.fromFloat (-1 * shieldRadius + radius)) ++ "px")
        -- , style "background-color" "rgba(0, 0, 200, 0.3)" -- Semi-transparent fill
        , style "box-sizing" "border-box"
        ]
        []

renderLaser : Entity -> Html Msg
renderLaser {radius} = 
    div [
        style "position" "absolute",
        style "left" ((String.fromFloat (radius * -7.5)) ++ "px"),
        style "width" ((String.fromFloat (radius * 15)) ++ "px"), 
        style "height" ((String.fromFloat (radius * 2)) ++ "px"),
        style "background-color" "red"
    ] []

renderExplosion : Entity -> Html Msg
renderExplosion {radius} = 
    div [
        -- style "position" "absolute",
        -- style "top" ((String.fromFloat (y - fradius / 2)) ++ "px"), 
        -- style "left" ((String.fromFloat (x - fradius / 2)) ++ "px"),
        style "width" ((String.fromFloat (radius * 2)) ++ "px"), 
        style "height" ((String.fromFloat (radius * 2)) ++ "px"),
        style "border-radius" "50%",
        style "background-color" "orange",
        style "opacity" "50%"
    ] []

renderFuelDepot : Entity -> Html Msg
renderFuelDepot {radius, supplyRadius, fuel, maxFuel} = 
    div [] [
        div [
            style "width" ((String.fromFloat (2 * radius)) ++ "px"), 
            style "height" ((String.fromFloat (2 * radius)) ++ "px"),
            style "border" "2px solid orange"
        ] [],
        div [
            style "position" "absolute",
            style "top" "2px", style "left" "2px",
            style "width" ((String.fromFloat (2 * radius)) ++ "px"), 
            style "height" (String.fromFloat ((toFloat fuel) / (toFloat maxFuel) * (2 * radius)) ++ "px"),
            style "background-color" "orange"
        ] [],
        div [
            style "position" "absolute",
            style "top" ((String.fromFloat (-1 * supplyRadius + radius)) ++ "px"), 
            style "left" ((String.fromFloat (-1 * supplyRadius + radius)) ++ "px"),
            style "width" ((String.fromFloat (supplyRadius * 2)) ++ "px"), 
            style "height" ((String.fromFloat (supplyRadius * 2)) ++ "px"),
            style "border-radius" "50%",
            style "border" "1px solid green"
        ] []
    ]

renderAmmoDepot : Entity -> Html Msg
renderAmmoDepot {radius, supplyRadius, ammo, maxAmmo} = 
    div [] [
        div [
            style "width" ((String.fromFloat (2 * radius)) ++ "px"), 
            style "height" ((String.fromFloat (2 * radius)) ++ "px"),
            style "border" "2px solid red"
        ] [],
        div [
            style "position" "absolute",
            style "top" "2px", style "left" "2px",
            style "width" ((String.fromFloat (2 * radius)) ++ "px"), 
            style "height" (String.fromFloat ((toFloat ammo) / (toFloat maxAmmo) * (2 * radius)) ++ "px"),
            style "background-color" "red"
        ] [],
        div [
            style "position" "absolute",
            style "top" ((String.fromFloat (-1 * supplyRadius + radius)) ++ "px"), 
            style "left" ((String.fromFloat (-1 * supplyRadius + radius)) ++ "px"),
            style "width" ((String.fromFloat (supplyRadius * 2)) ++ "px"), 
            style "height" ((String.fromFloat (supplyRadius * 2)) ++ "px"),
            style "border-radius" "50%",
            style "border" "1px solid green"
        ] []
    ]


renderBoundingCircle : Entity -> Html msg
renderBoundingCircle {radius} =
    div
        [ style "position" "absolute"
        , style "border" "2px dashed red"
        , style "border-radius" "50%"
        , style "width" (String.fromFloat (radius * 2) ++ "px")
        , style "height" (String.fromFloat (radius * 2) ++ "px")
        , style "top" "0px"
        , style "left" "0px"
        , style "background-color" "rgba(255, 0, 0, 0.3)" -- Semi-transparent fill
        , style "box-sizing" "border-box"
        ]
        []

noShape : Html msg
noShape = span [] []



--------------------------------------------------------------------------------
-------------------------- ENTITIES --------------------------------------------
defaultEntity : Entity
defaultEntity = {
    id = 0, name = "",
    x = 0, y = 0, 
    speed = 0, maxSpeed = 0,
    theta = 0, thetaSpeed = 0,
    accel = 0, 
    radius = 0,

    isShortLived = False,
    ticksLeft = 0,

    isCollided = False,
    collidedWith = "",

    isDestroyed = False,
    isPlayerControlled = False,
    
    isFueled = False,
    isFuelDepot = False,
    fuel = 0, maxFuel = 0,
    isArmed = False,
    isAmmoDepot = False,
    ammo = 0, maxAmmo = 0,
    supplyRadius = 0,

    isShielded = False,
    shieldRadius = 0,
    shieldHP = 0
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
    x = x, y = y, 
    radius = 8,
    name = "Ship",
    maxSpeed = 4,
    isFueled = True,
    fuel = 300, maxFuel = 300,

    isArmed = True,
    ammo = 5, maxAmmo = 10,

    isPlayerControlled = True,

    isShielded = True,
    shieldRadius = 20,
    shieldHP = 1
    }

makeAsteroid : Float -> Float -> Float -> Float -> Entity 
makeAsteroid x y speed theta = {defaultEntity | 
    x = x, y = y, 
    radius = 10,
    name = "Asteroid",
    speed = speed,
    theta = theta,
    maxSpeed = 2
    }

makeBigAsteroid : Float -> Float -> Float -> Float -> Entity 
makeBigAsteroid x y speed theta = {defaultEntity | 
    x = x, y = y, 
    radius = 25,
    name = "Big Asteroid",
    speed = speed,
    theta = theta,
    maxSpeed = 1.5
    }

makeLaser : Float -> Float -> Float -> Entity
makeLaser x y theta = {defaultEntity |
    x = x, y = y, 
    radius = 1,
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
        radius = 1,
        ticksLeft = ticks,
        isShortLived = True
    }

makeFuelDepot : Float -> Float -> Float -> Float -> Entity
makeFuelDepot x y speed theta = {defaultEntity |
    name = "Fuel Depot",
    x = x, y = y, 
    radius = 10,
    supplyRadius = 100,
    speed = speed,
    theta = theta,
    maxSpeed = 1,
    fuel = 1000, maxFuel = 1000,
    isFuelDepot = True,

    isShielded = True,
    shieldRadius = 45,
    shieldHP = 1
    }

makeAmmoDepot : Float -> Float -> Float -> Float -> Entity
makeAmmoDepot x y speed theta = {defaultEntity |
    name = "Ammo Depot",
    x = x, y = y, 
    radius = 10,
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
    |> (\ mod -> {mod | entities = shieldCollisions mod.entities})
    |> (\ mod -> {mod | entities = destroyCollisions mod.entities})
    |> makeExplosions
    |> (\ mod -> {mod | entities = stepExplosions mod.entities})
    |> makeAsteroids
    |> (\ mod -> {mod | entities = destroyEntities mod.entities})
    |> checkGameOver


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
        Just eb -> {ea | isCollided = True, collidedWith = eb.name} 
            :: computeCollisions (setEntity eb rest)

collidesAny : Entity -> List Entity -> Maybe Entity
collidesAny entity entities = case entities of
    [] -> Nothing
    e :: rest -> if collides entity e 
        then Just { e | isCollided = True, collidedWith = entity.name }
        else collidesAny entity rest

collides : Entity -> Entity -> Bool
collides ea eb =
    let 
        a1 = if ea.isShielded && ea.shieldHP > 0 then ea.shieldRadius else ea.radius
        b1 = if eb.isShielded && eb.shieldHP > 0 then eb.shieldRadius else eb.radius
        -- (aradius, bradius) = if ea.isShielded && eb.isShielded then (ea.radius, eb.radius) else (a1, b1)
    in
        sqrt ((eb.x - ea.x) ^ 2 + (eb.y - ea.y) ^ 2) < a1 + b1


shieldCollisions : EntitySystem 
shieldCollisions entities = map (\ e -> 
    case e.collidedWith of
        "Asteroid" -> if e.name /= "Asteroid" && e.shieldHP > 0
            then {e | shieldHP = e.shieldHP - 1, isCollided = False, collidedWith = ""} 
            else e
        "Laser" -> if e.name /= "Ship" && e.shieldHP > 0
            then {e | shieldHP = e.shieldHP - 1, isCollided = False, collidedWith = ""} 
            else e
        "Ship" -> if e.name /= "Laser" && e.shieldHP > 0
            then {e | shieldHP = e.shieldHP - 1, isCollided = False, collidedWith = ""} 
            else e
        "Explosion" -> e
        "" -> e
        _ -> if e.shieldHP > 0
            then {e | shieldHP = e.shieldHP - 1, isCollided = False, collidedWith = ""} 
            else e
    ) entities
    

destroyCollisions : EntitySystem
destroyCollisions entities = map (\ e -> 
    case e.collidedWith of
        "Asteroid" -> if e.name /= "Asteroid" 
            then {e | isDestroyed = True} 
            else {e | isCollided = False, collidedWith = ""}
        "Laser" -> if e.name /= "Ship" 
            then {e | isDestroyed = True} 
            else {e | isCollided = False, collidedWith = ""}
        "Ship" -> if e.name /= "Laser" 
            then {e | isDestroyed = True} 
            else {e | isCollided = False, collidedWith = ""}
        "Explosion" -> {e | isCollided = False, collidedWith = ""}
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

stepExplosions : EntitySystem 
stepExplosions entities = map (\e -> case e.name of 
        "Explosion" -> { e | radius = e.radius + 0.5 }
        _ -> e
    ) entities

makeAsteroids : ModelSystem
makeAsteroids ({nextId, entities} as model) =
    let 
        asteroids = filter 
            (\e -> e.name == "Big Asteroid" && e.isDestroyed) entities
            |> foldl (\ e es -> 
                (makeAsteroid e.x e.y 1 (e.theta * 0.5)) :: 
                (makeAsteroid e.x e.y 1.1 (e.theta * 1.5)) :: es) []
            |> indexedMap (\ i e -> { e | id = nextId + i})
    in { model |
        nextId = length asteroids + nextId,
        entities = asteroids ++ entities
    }


destroyEntities : EntitySystem
destroyEntities entities = filter (\e -> e.isDestroyed == False) entities

checkGameOver : ModelSystem
checkGameOver ({entities} as model) = 
    let 
        lost = not (any (\e -> e.name == "Ship") entities)
        won = not (any (\e -> e.name == "Asteroid" || e.name == "Big Asteroid") entities)
    in
        {model | 
            isGameLost = lost,
            isGameWon = won,
            paused = won || lost
        }

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