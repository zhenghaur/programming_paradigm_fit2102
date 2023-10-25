import "./style.css";

import { interval, fromEvent, zip, merge } from "rxjs";
import { map, filter, scan } from "rxjs/operators";


function main() {
  /**
   * Inside this function you will use the classes and functions from rx.js
   * to add visuals to the svg element in pong.html, animate them, and make them interactive.
   *
   * Study and complete the tasks in observable examples first to get ideas.
   *
   * Course Notes showing Asteroids in FRP: https://tgdwyer.github.io/asteroids/
   *
   * You will be marked on your functional programming style
   * as well as the functionality that you implement.
   *
   * Document your code!
   */

  /**
   * This is the view for your game to add and update your game elements.
   */
  const svg = document.querySelector("#svgCanvas") as SVGElement & HTMLElement;

  // classes for objects initialized by streams for different functionality
  class Move {constructor(public readonly x: number, public readonly y: number){}}
  class Tick {constructor(public readonly elapsed: number) {}}
  class Restart {}

  // the observables of the game
  const 
    key$ = fromEvent<KeyboardEvent>(document, "keydown"),
    up = key$.pipe(filter(event => event.key == "w" || event.keyCode ==38), map(_=> new Move(0, -50))),
    down = key$.pipe(filter(event => event.key == "s" || event.keyCode ==40), map(_=> new Move(0, 50))),
    left = key$.pipe(filter(event => event.key == "a" || event.keyCode ==37), map(_=> new Move(-25, 0))),
    right = key$.pipe(filter(event => event.key == "d" || event.keyCode ==39), map(_=> new Move(25, 0))),
    space = key$.pipe(filter(event => event.key == "r"), map(_=> new Restart())),
    gameClock = interval(10).pipe(map(t => new Tick(t)))


  // defining the types used in the game
  type State = Readonly<{
    time: number,
    frog:Body,
    cars: Car[],
    planks: Plank[],
    crocs: Body[],
    turtles: Turtle[],
    targets: Target[],
    gameOver:boolean,
    score: number
    highscore: number[]
  }>

  type Body = Readonly<{
    id: string
    pos_x: number
    pos_y: number
  }>

  type Car = Readonly<{
    row: number
    body: Body,
    speed: number,
    color: string
  }>

  type Plank = Readonly<{
    row: number
    body: Body,
    length: number
  }>

  type Turtle = Readonly<{
    body: Body,
    isSafe: Boolean
  }>

  type Target = Readonly<{
    body: Body,
    isReached: Boolean
  }>

  // functions to create each "objects" of the game
  function createFrog() : Body{
    return {
      id: "frog",
      pos_x: 300,
      pos_y: 575
    }
  }

  function createCar(row:number, id:string, pos_x:number, pos_y:number, speed:number, color:string): Car {
    return {
      row: row,
      body: {id: id, pos_x: pos_x, pos_y: pos_y},
      speed: speed,
      color: color
    }
  }

  function createPlank(row:number, id:string, pos_x:number, pos_y:number, length:number): Plank {
    return {
      row: row,
      body: {id: id, pos_x: pos_x, pos_y: pos_y},
      length: length
    }
  }

  function createCroc(id:string, pos_x: number, pos_y:number):Body {
    return {
      id: id,
      pos_x :pos_x,
      pos_y: pos_y
    }
  }

  function createTurtle(id:string, pos_x: number, pos_y: number, isSafe: boolean): Turtle {
    return {
      body: {id: id, pos_x: pos_x, pos_y: pos_y},
      isSafe: isSafe
    }
  }

  function createTarget(id: string, pos_x: number, pos_y: number): Target{
    return {
      body: {id: id, pos_x: pos_x, pos_y: pos_y},
      isReached: false
    }
  }

  // initializing the state of the game 
  const targets = <Target[]>[createTarget("target1", 100, 25),
                           createTarget("target2", 200, 25),
                           createTarget("target3", 300, 25),
                           createTarget("target4", 400, 25),
                           createTarget("target5", 500, 25),]

  const planks = <Plank[]>[createPlank(5, "plank1", 0, 255, 200),
                           createPlank(5, "plank2", 400, 255, 250),
                           createPlank(6, "plank3", 50, 205, 150),
                           createPlank(6, "plank4", 350, 205, 150),
                           createPlank(6, "plank5", -250, 205, 150),
                           createPlank(7, "plank6", 0, 155, 200),
                           createPlank(7, "plank7", 300, 155, 150),
                           createPlank(7, "plank8", -250, 155, 100)]

  const cars = <Car[]>[createCar(4, "car1", 50, 357, 1, "darkorange"),
                       createCar(4, "car2", 400, 357, 1, "darkorchid"),
                       createCar(3, "car3", 150, 407, 2, "lightbluesky"),
                       createCar(3, "car4", 500, 407, 2, "hotpink"),
                       createCar(2, "car5", 50, 457, -2, "mediumblue"),
                       createCar(2, "car6", 400, 457, -2, "gold"),
                       createCar(1, "car7", 150, 507, -1, "lightseagreen"),
                       createCar(1, "car8", 500, 507, -1, "darksalmon")]

  const crocs = <Body[]>[createCroc("croc1", 0, 105),
                        createCroc("croc2", 250, 105),
                        createCroc("croc3", 500, 105),]

  const turtles = <Turtle[]>[createTurtle("turtle1", 25, 75, true), createTurtle("turtle2", 75, 75, true),
                             createTurtle("turtle3", 275, 75, false), createTurtle("turtle4", 325, 75, false),
                             createTurtle("turtle5", 475, 75, false), createTurtle("turtle6", 525, 75, false), createTurtle("turtle7", 575, 75, false)]


  const initialState : State = {
    time: 0,
    frog: createFrog(),
    cars: cars,
    planks: planks,
    crocs: crocs,
    turtles: turtles,
    targets: targets,
    gameOver: false,
    score: 0,
    highscore: []
  }

  // adapted from FRP Asteroids
  // function to update the view of the game
  const updateState = (currentState: State) => {
    // functions to create svg elements if not created yet
    const getCar = (car: Car) => {
      const c = document.createElementNS(svg.namespaceURI, "rect");
        c.setAttribute("id", car.body.id);
        c.setAttribute("rx", "5");
        c.setAttribute("ry", "5");
        c.setAttribute("height", "36");
        c.setAttribute("width", "60");
        c.setAttribute("x", String(car.body.pos_x));
        c.setAttribute("y", String(car.body.pos_y));
        c.setAttribute("style", "fill: " + car.color + ";")
        svg.appendChild(c);
        return c;
    }
    
    const getPlank = (plank:Plank) => {
        const c = document.createElementNS(svg.namespaceURI, "rect");
        c.setAttribute("id", plank.body.id);
        c.setAttribute("rx", "3");
        c.setAttribute("ry", "3");
        c.setAttribute("height", "40");
        c.setAttribute("width", String(plank.length));
        c.setAttribute("x", String(plank.body.pos_x));
        c.setAttribute("y", String(plank.body.pos_y));
        c.setAttribute("style", "fill: saddlebrown;")
        svg.appendChild(c);
        return c;
      }

    const getFrog = (frog: Body) => {
      const f = document.createElementNS(svg.namespaceURI, "circle");
      f.setAttribute("id", frog.id);
      f.setAttribute("r", "15");
      f.setAttribute("cx", String(frog.pos_x));
      f.setAttribute("cy", String(frog.pos_y));
      f.setAttribute(
        "style",
        "fill: greenyellow; stroke: forestgreen; stroke-width: 5px;"
      );
      svg.appendChild(f)
      return f;
    }

    const getCrocHead = (croc: Body) => {
      const h = document.createElementNS(svg.namespaceURI, "rect");
      h.setAttribute("id", croc.id+"head");
      h.setAttribute("height", "40");
      h.setAttribute("width", "50");
      h.setAttribute("x", String(croc.pos_x));
      h.setAttribute("y", String(croc.pos_y));
      h.setAttribute(
        "style",
        "fill: red;"
      );
      svg.appendChild(h);
      return h;
    }

    const getCrocTail = (croc:Body) => {
      const t = document.createElementNS(svg.namespaceURI, "rect");
      t.setAttribute("id", croc.id+"tail");
      t.setAttribute("height", "40");
      t.setAttribute("width", "100");
      t.setAttribute("x", String(croc.pos_x+50));
      t.setAttribute("y", String(croc.pos_y));
      t.setAttribute(
        "style",
        "fill: olive;"
      );
      svg.appendChild(t);
      return t;
    }

    const getTurtle = (turtle: Turtle) => {
      const t = document.createElementNS(svg.namespaceURI, "circle");
      t.setAttribute("id", turtle.body.id);
      t.setAttribute("r", "22");
      t.setAttribute("cx", String(turtle.body.pos_x));
      t.setAttribute("cy", String(turtle.body.pos_y));
      svg.appendChild(t)
      return t;
    }

    const getTarget = (target: Target) => {
      const t = document.createElementNS(svg.namespaceURI, "circle");
      t.setAttribute("id", target.body.id);
      t.setAttribute("r", "15");
      t.setAttribute("cx", String(target.body.pos_x));
      t.setAttribute("cy", String(target.body.pos_y));
      t.setAttribute(
        "style",
        "fill: forestgreen;"
      );
      svg.appendChild(t)
      return t;
    }

    const getGameOver = () => {
      const v = document.createElementNS(svg.namespaceURI, "text")!;
      v.setAttribute("id", "gameOver");
      v.setAttribute("x", "100");
      v.setAttribute("y", "300");
      v.setAttribute("font-size", "30px")
      v.textContent = "";
      svg.appendChild(v);
      return v
    }

    // functions to update the attributes of the svg elements
    const moveCar = (car: Car) => {
        const c = document.getElementById(car.body.id) || getCar(car);
        c.setAttribute("x", String(car.body.pos_x));
      }
      
    const movePlank = (plank: Plank) => {
      const p = document.getElementById(plank.body.id) || getPlank(plank);
      p.setAttribute("x", String(plank.body.pos_x));
    }
    
    const moveFrog = (frog: Body) => {
      const f = document.getElementById(frog.id) || getFrog(frog);
      f.setAttribute("cx", String(frog.pos_x));
      f.setAttribute("cy", String(frog.pos_y));
    }

    const moveTurtle = (turtle: Turtle) => {
      const t = document.getElementById(turtle.body.id) || getTurtle(turtle);
      t.setAttribute("cx", String(turtle.body.pos_x));
      turtle.isSafe? t.setAttribute("style", "fill: seagreen;"): t.setAttribute("style", "fill: darkturquoise;");
    }

    const moveCrocHead = (croc: Body) => {
      const c = document.getElementById(croc.id + "head") || getCrocHead(croc);
      c.setAttribute("x", String(croc.pos_x));
    }

    const moveCrocTail = (croc: Body) => {
      const c = document.getElementById(croc.id + "tail") || getCrocTail(croc);
      c.setAttribute("x", String(croc.pos_x + 50));
    }

    const colorTarget = (target: Target) => {
      const t = document.getElementById(target.body.id) || getTarget(target);
      target.isReached ? t.setAttribute("style", "fill: forestgreen;"):t.setAttribute("style", "fill: red;")
    }

    // updating the svg elements according to the state of the game
    currentState.cars.forEach(moveCar);
    currentState.planks.forEach(movePlank);
    currentState.crocs.forEach(moveCrocHead);
    currentState.crocs.forEach(moveCrocTail);
    currentState.turtles.forEach(moveTurtle);
    currentState.targets.forEach(colorTarget);
    moveFrog(currentState.frog);
    
    const g = document.getElementById("gameOver") || getGameOver();
    (currentState.gameOver) ? g.textContent = "Game Over, press 'r' to restart." : g.textContent = "";

    // updating html elements to the state of the game
    const score = document.getElementById("score")!;
    const s = "Score: " + String(currentState.score);
    score.innerHTML = s;

    const highscore = document.getElementById("highscore")!;
    const hs = "Highscore: " + currentState.highscore.join('     ')
    highscore.innerHTML = hs;
    
  }

  // adapted from FRP Asteroids
  // tick function to update the state of the game as time passes
  function tick (currentState: State):State {
    // passing the new state into handleCollision
    return handleCollision({
      ...currentState,
      time: currentState.time + 1,
      // moving the car
      cars: currentState.cars.map(car => ({...car, body: {
        ...car.body, 
        pos_x: car.speed > 0? (car.body.pos_x + car.speed > 600? -100 : car.body.pos_x + car.speed) :(car.body.pos_x + car.speed < -100? 600 : car.body.pos_x + car.speed)
      }})),
      // moving the planks
      planks: currentState.planks.map(plank => ({...plank, body: {
        ...plank.body, 
        pos_x: plank.body.pos_x + plank.row/5*0.8 > 600 ? -250 : plank.body.pos_x + plank.row/5*0.8
      }})),
      // moving the crocodiles
      crocs: currentState.crocs.map(croc => ({
        ...croc,
        pos_x: croc.pos_x < -150 ? 600: croc.pos_x - 1
      })),
      // moving the turtles
      turtles: currentState.turtles.map(turtle => ({
        ...turtle,
        body: {...turtle.body, pos_x: turtle.body.pos_x < -125? 625: turtle.body.pos_x - 0.5},
        isSafe: currentState.time % 400 == 0 ? !turtle.isSafe : turtle.isSafe
      }))
    })
  }

  // adapted from FRP Asteroids
  // function to calculate the collision of elements
  function handleCollision (currentState: State) :State {
    // function to calculate if frog and car is colliding
    const carCollision = ([a,b]: [Body, Car]) => {
      return (a.pos_x + 20 > b.body.pos_x && a.pos_x - 20 < b.body.pos_x + 60 && a.pos_y + 20 > b.body.pos_y && a.pos_y - 20 < b.body.pos_y + 36)
    }
    // function to calculate if frog and plank is colliding
    const plankCollision = ([a,b]: [Body, Plank]) => {
      return (a.pos_x > b.body.pos_x && a.pos_x < b.body.pos_x + b.length && a.pos_y > b.body.pos_y && a.pos_y < b.body.pos_y + 40)
    }
    // function to calculate if frog and crocodile is colliding
    const crocCollision = ([a,b]: [Body, Body]) => {
      return (a.pos_x > b.pos_x+ 50 && a.pos_x < b.pos_x + 150 && a.pos_y > b.pos_y && a.pos_y < b.pos_y + 40)
    }
    // function to calculate if frog and turtle is colliding
    const turtleCollision = ([a,b]: [Body, Turtle]) => {
      return (b.isSafe && a.pos_x > b.body.pos_x - 25 && a.pos_x < b.body.pos_x + 25 && a.pos_y > b.body.pos_y - 20 && a.pos_y < b.body.pos_y + 20)
    }
    // function to calculate if frog and target is colliding
    const targetCollision = ([a,b]: [Body, Target]) => {
      return (a.pos_x + 20 > b.body.pos_x - 20 && a.pos_x - 20 < b.body.pos_x + 20 && a.pos_y + 20 > b.body.pos_y - 20 && a.pos_y - 20 < b.body.pos_y + 20 )
    }
    
    // array of all colliding elements with frog 
    const collidedCar = currentState.cars.filter(car => carCollision([currentState.frog, car]));
    const collidedPlank = currentState.planks.filter(plank => plankCollision([currentState.frog, plank]));
    const collidedCroc = currentState.crocs.filter(croc => crocCollision([currentState.frog, croc]));
    const collidedTurtle = currentState.turtles.filter(turtle => turtleCollision([currentState.frog, turtle]));
    const collidedRiver = !(collidedPlank.length > 0 || collidedCroc.length > 0 || collidedTurtle.length>0) && currentState.frog.pos_y> 50 && currentState.frog.pos_y < 300;
    const collidedTarget = currentState.targets.filter(target => targetCollision([currentState.frog, target]));
    const notCollidedTarget = currentState.targets.filter(target => !targetCollision([currentState.frog, target]));

    // updating the state of the game according to the collision of the elements
    return {
      ...currentState,
      frog: (collidedTarget.length > 0)?
                      createFrog()
                      :((collidedPlank.length > 0)? 
                                {...currentState.frog, pos_x: currentState.frog.pos_x + collidedPlank[0].row/5*0.8}
                                :((collidedCroc.length > 0) ?
                                          {...currentState.frog, pos_x: currentState.frog.pos_x - 1}
                                          :(collidedTurtle.length > 0)?
                                                    {...currentState.frog, pos_x: currentState.frog.pos_x - 0.5}
                                                    :currentState.frog)),
      gameOver: collidedCar.length>0 || collidedRiver,
      targets: notCollidedTarget.concat(collidedTarget.map(target => <Target>{...target, isReached: true})),
      score: collidedTarget.length > 0 && !collidedTarget[0].isReached ? currentState.score + 100 : currentState.score,
      highscore: (collidedCar.length>0 || collidedRiver)? 
                          (currentState.highscore.length==5?
                                    (currentState.highscore[4]>currentState.score?
                                              currentState.highscore: 
                                              currentState.highscore.slice(0,4).concat(currentState.score).sort().reverse())
                                    :currentState.highscore.concat(currentState.score)).sort().reverse()
                          :currentState.highscore
    }
  }

  // adapted from FRP Asteroids
  // function to transform the state of the game according to the object passed in (move/tick/restart)
  function reduceState (currentState: State, e: Move|Tick|Restart) : State {
    return !currentState.gameOver?(e instanceof Move? ({
      ...currentState, 
      frog: <Body>{id: currentState.frog.id, 
                   pos_x: currentState.frog.pos_x + e.x, 
                   pos_y: currentState.frog.pos_y + e.y}
    }) : tick(currentState)):(e instanceof Restart?({...initialState, highscore: currentState.highscore}):currentState)
  }

  // adapted from FRP Asteroids
  // the main subscription of the game
  const subscription = merge(up, down, left, right, space, gameClock)
                      .pipe(
                        scan(reduceState, initialState)
                      ).subscribe(updateState);
  

  }



// The following simply runs your main function on window load.  Make sure to leave it in place.
if (typeof window !== "undefined") {
  window.onload = () => {
    main();
  };
}
