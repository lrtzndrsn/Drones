open Simulate

type BlackBoxTest() =
    static member testPosition() =
        printfn
            "Hello and welcome to this extraordinary Blackbox test of \"Simulate.fs\" \nFirst we will test the Drone-class:"

        printfn ""
        printfn "Test of this.Position:"
        let drone1 = Drone((2, 2), (5, 5), 2)
        printfn "Start position of drone1 (Positive start position): %b" (drone1.Position = (2, 2))
        let drone2 = Drone((-2, -3), (3, 5), 2)
        printfn "Start position of drone2 (Negative start position): %b" (drone2.Position = (-2, -3))

    static member testSpeed() =
        printfn ""
        printfn "Test of this.Speed:"
        let drone1 = Drone((2, 2), (5, 5), 2)
        printfn "Speed of Drone1 (Positive speed): %b" (drone1.Speed = (2))
        let drone2 = Drone((-1, -3), (1, 3), -5)
        printfn "Speed of Drone1 (Negative speed): %b" (drone2.Speed = (-5))
        let drone3 = Drone((-1, -3), (1, 3), 0)
        printfn "Speed of Drone1 (No speed): %b" (drone3.Speed = (0))

    static member testDestination() =
        printfn ""
        printfn "Test of this.Destination:"
        let drone1 = Drone((2, 2), (5, 5), 2)
        printfn "Destination of Drone1 (Positive Destination): %b" (drone1.Destination = (5, 5))
        let drone2 = Drone((-1, -3), (-5, -5), -5)
        printfn "Destination of Drone2 (Negative Destination): %b" (drone2.Destination = (-5, -5))
        let drone3 = Drone((-1, -3), (0, 0), -5)
        printfn "Destination of Drone3 (No Destination): %b" (drone3.Destination = (0, 0))

    static member testAtDestination() =
        printfn ""
        printfn "Test of this.atDestination:"
        let drone1 = Drone((5, 5), (5, 5), 2)
        let drone2 = Drone((5, 5), (3, 1), 2)
        printfn "Drone is at destination: %b" (drone1.atDestination () = true)
        printfn "Drone is not at destination: %b" (drone2.atDestination () = false)

    static member testFly() =
        printfn ""
        printfn "Test of this.Fly"
        let drone1 = Drone((2, 2), (5, 5), 2)
        let drone2 = Drone((0, 0), (5, 5), 10)
        let drone3 = Drone((-5, -5), (5, 5), 4)
        let drone4 = Drone((0, 0), (5, 5), 400)
        let drone5 = Drone((0, 0), (10, 10), -2)
        drone1.Fly()
        drone2.Fly()
        drone3.Fly()
        drone4.Fly()
        drone5.Fly()
        printfn "drone1 (Positive values) Position after one second flight: %b" (drone1.Position = (3, 3))
        printfn "drone2 (Start in 0,0) Position after one second flight: %b" (drone2.Position = (5, 5))
        printfn "drone3 (Negative start position) Position after one second flight: %b" (drone3.Position = (-3, -3))
        printfn "drone4 (Speed larger than distance) Position after one second flight: %b" (drone4.Position = (5, 5))
        printfn "drone5 (Negative speed) Position after one second flight: %b" (drone5.Position = (-1, -1))


    static member DroneDist() =
        printfn ""
        printfn "The following is a test of the \"Airspace-Class\"."
        printfn ""
        printfn "Test of \"this.DroneDist\"."
        let drone1 = Drone((0, 0), (10, 10), 2)
        let drone2 = Drone((0, 0), (10, 10), 2)
        let drone3 = Drone((0, 10), (10, 10), 2)
        let drone4 = Drone((5, 10), (10, 10), 2)
        let drone5 = Drone((0, 5), (10, 10), 2)
        let drone6 = Drone((0, 6), (10, 10), 2)
        let distSpace = Airspace [ drone1; drone2; drone3; drone4; drone5; drone6 ]

        printfn
            "Distance between two identical drones (drone1 and drone2) = 0: %b"
            (distSpace.DroneDist (drone1) (drone2) = 0)

        printfn "Distance in x between drone3 and drone 4 = 5: %b" (distSpace.DroneDist (drone3) (drone4) = 5)
        printfn "Distance in y between drone5 and drone6 = 1: %b" (distSpace.DroneDist (drone5) (drone6) = 1)

        printfn
            "Distance in x and y between two entirely different drones (Drone4 and Drone 6 = 21): %b"
            (distSpace.DroneDist (drone4) (drone6) = 21)

    static member FlyDronesTest() =
        printfn ""
        printfn "Test of \"this.FlyDrones\"."
        let drone1 = Drone((0, 0), (10, 10), 2)
        let drone2 = Drone((1, 1), (5, 5), 5)
        let flySpace = Airspace [ drone1; drone2 ]
        flySpace.FlyDrones()

        printfn
            "FlyDrones called on two non-identical drones: %b"
            ((drone1.Position, drone2.Position) = ((1, 1), (5, 5)))

    static member AddDroneTest() =
        printfn ""
        printfn "Test of \"this.AddDroneTest\"."
        let droneSpace = Airspace []
        printfn "Empty Airspace (0 Elements in list): %b" (droneSpace.Drones.Length = 0)
        droneSpace.AddDrone(Drone((0, 0), (10, 10), 2))
        printfn "Added one drone in list. (1 Element in list): %b" (droneSpace.Drones.Length = 1)
        droneSpace.AddDrone(Drone((5, 5), (2, 5), 5))
        printfn "Added second drone in list. (2 Elements in list): %b" (droneSpace.Drones.Length = 2)
        droneSpace.AddDrone(Drone((5, 8), (10, 5), 7))
        printfn "Added third drone in list. (3 Elements in list): %b" (droneSpace.Drones.Length = 3)

    static member WillCollideTest() =
        printfn ""
        printfn "Test of \"this.WillCollide\"."
        let drone1 = Drone((0, 0), (10, 10), 5)
        let drone2 = Drone((0, 0), (10, 10), 5)
        let drone3 = Drone((5, 5), (9, 9), 20)
        let drone4 = Drone((2, 2), (9, 9), 5)
        let drone5 = Drone((10, 10), (20, 55), 10)
        let drone6 = Drone((10, 10), (30, 20), 15)
        let collideSpace1 = Airspace [ drone1; drone2 ]
        let collideSpace2 = Airspace [ drone3; drone4 ]
        let collideSpace3 = Airspace [ drone5; drone6 ]
        let collideSpace4 = Airspace [ drone1; drone3 ]

        printfn
            "Drones with identical start position, destination and speed. Positive Time = 5: %b"
            ((collideSpace1.WillCollide(5)).Length = 2)

        printfn
            "Drones with identical start position, destination and speed. Negative time = -20: %b"
            ((collideSpace1.WillCollide(-20)).Length = 0)

        printfn
            "Drones with different start positions, identical destinations, different speeds. Time = 5: %b"
            ((collideSpace2.WillCollide(5)).Length = 1)

        printfn
            "Drones with different start positions, identical destinations, different speeds. Time = 69: %b "
            ((collideSpace2.WillCollide(69)).Length = 0)

        printfn
            "Drones with identical start positions, different destinations, different speeds. Time = 5: %b"
            ((collideSpace3.WillCollide(5)).Length = 2)

        printfn
            "Drones with different start positions, different destinations, different speeds. Time = 0: %b"
            ((collideSpace4.WillCollide(0)).Length = 0)

        printfn
            "Drones with identical start positions, different destinations, different speeds. Time = 20: %b"
            ((collideSpace4.WillCollide(20)).Length = 1)

    static member RunAllTests() =
        BlackBoxTest.testPosition ()
        BlackBoxTest.testSpeed ()
        BlackBoxTest.testDestination ()
        BlackBoxTest.testAtDestination ()
        BlackBoxTest.testFly ()
        BlackBoxTest.DroneDist()
        BlackBoxTest.FlyDronesTest()
        BlackBoxTest.AddDroneTest()
        BlackBoxTest.WillCollideTest()

BlackBoxTest.RunAllTests()
