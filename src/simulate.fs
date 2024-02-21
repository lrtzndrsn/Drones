module Simulate 

//<summary> The Drone-class takes a position of int*int, a destination of int*int and a speed of int in the constructor. When the instance of the class is called, 
/// a drone-unit is created. <\summary>
///<param name>"pos" - accepts a position of int*int (an (x,y) coordinate.)  <\param name>
///<param name> "dest" - accepts a destination of int*int (an (x,y) coordinate.)  <\param name>
///<param name> "speed" - accept an int and represents speed pr second in centimeters.  <\param name>
///<returns> Returns a unit, when the object is created.  </returns>

type Drone (pos:int*int, dest:int*int, speed:int) = 
    let mutable position = pos 
    member this.Position // Position represented as int * int coordinate.
        with get () = position
        and set (value) = position <- value 
    member this.Speed = speed // speed represented as an integer.
    member this.Destination = dest // The destination represented as int*int coordinates
    member this.atDestination () = this.Position = this.Destination // Takes a unit (A drone) and returns a bool, if the drone is at its destination
    member this.Fly () =    // takes a unit (a Drone) and returns a new unit (A drone which position has been changed.)
        let (x,y) = float(fst dest - fst position), (float(snd dest - snd position))  // Direction vector from position to destination. 
        let angle = atan2 y x // Angle of direction vector. 
        let newxy = (sin(angle) * float(speed)), (cos(angle) * float(speed)) // Coordinates of angle*speed in x direction and y direction. 
        let flyPos = (fst position + int (fst newxy), snd position + int (snd newxy)) //Fly Position
        let distance = int (sqrt((float (fst position- fst dest)**2.0) + (float(snd position - snd dest)**2.0))) //Distance between start position and destination.
        if  speed >= distance then 
            position <- dest 
        else  
            position <- flyPos 

//<summary> The Airspace-class takes a drone-list. When the instance of the class is called, an Airspace-unit is created. <\summary>
///<param name>"dronelist" - accepts a list of drones <\param name>
///<returns> Returns a unit, when the object is created.  </returns>
type Airspace (droneList: Drone list) = 
    let mutable drones = droneList
    
    member this.Drones = drones

//<summary> The DroneDist-member takes two drones, calculates the distance between them and returns the distance.  <\summary>
///<param name>"drone1", accepts a drone <\param name>
///<param name>"drone2", accepts a drone <\param name>
///<returns> Returns a integer, corresponding to the distance, when the object is created.  </returns>
    member this.DroneDist (drone1: Drone) (drone2: Drone) : int = 
        let pos1 = drone1.Position
        let pos2 = drone2.Position
        int ((sqrt(float (fst pos1- fst pos2)**2.0) + (float(snd pos1 - snd pos2)**2.0)))

//<summary> FlyDrones takes a list of drones, and calls this.Fly on the drones in the list. <\summary>
///<param name>"drone": Accepts any drone
///<returns> Returns a unit  </returns>
    member this.FlyDrones () = List.iter (fun (drone : Drone) -> drone.Fly()) <| drones

//<summary> AddDrone takes a drone and adds it to an Airspace. <\summary>
///<param name>"drone1": Accepts any drone
///<returns> Returns a unit. </returns>
    member this.AddDrone (drone1: Drone) : unit = 
        drones <- drone1 :: drones 

//<summary> WillCollide takes time as an input, given as an integer. It returns a list of Drone * Drone. It checks if drones will collide. <\summary>
///<param name>"time" accepts an integer
///<returns> Returns a list of Drone * Drone </returns>
    member this.WillCollide (time: int) =
        let mutable crashed = [] 
        for k = 0 to time * 60 - 1 do 
            this.FlyDrones() 
            for i in drones do 
                for j in drones do
                    if (this.DroneDist i j) < 500 && not (i.atDestination()) && not (j.atDestination()) then
                        crashed <- (i,j) :: crashed
                        drones <- List.filter(fun (x:Drone) -> not (x = i) && not (x = j)) drones 
        crashed 
