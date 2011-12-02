namespace Puzzle.Core

open System

// coordinate row * column
type Coordinate = int*int
type Tile = int
type Tiles = Tile array
// I think puzzle state is an array of columns
type PuzzleState = (Tile array) array

module Core =

    //therefore dim is the number of columns
    let dim (state : PuzzleState) = Array.length state

    let index2coord (dim : int) (ind : int) : Coordinate =
        ( ind / dim, ind % dim)

    let coord2index (dim : int) ((r, c) : Coordinate) : int =
        r * dim + c

    /// get the tile number of a PuzzleState by coordinate
    let getTile (state : PuzzleState) ((row,col) : Coordinate) : Tile =
        state.[row].[col]


    // we always have one empty tile in the game, and we assign that one the highest number,
    // which doesn't exist on the field, so no conflict here.
    let emptyTile (dim : int) : Tile =
        dim*dim

    /// checks if the tile at a given coordinate is empty
    /// by comparing the number with the number of the empty tile
    let isCoordEmpty (state : PuzzleState) ((r,c) : Coordinate) : bool =
        getTile state (r,c) = emptyTile(dim state)


    /// finds the coodrdinate of the empty tile using isCoordEmpty
    /// fails if there is not exaclty one position with an empty tile
    let emptyCoord (state : PuzzleState) : Coordinate =
        let dim = dim state
        let coords = 
            [   for r in [0..dim - 1] do
                for c in [0..dim - 1] do
                yield (r,c) 
            ]
            |> List.filter (isCoordEmpty state)

        match coords with
        | [x] -> x
        | _ -> failwith "multiple or no empty tiles found..."



    /// given a coordinate, get all surrounding coordinates
    /// this will be used to find the empty cells coordinates
    /// if the user clicks on a tile
    let getSurrounding (dim : int) ((r,c) : Coordinate) : Coordinate list =
        /// checks if a coordinate is inside the puzzle
        let isCoordInField (dim : int) ((r,c) : Coordinate) : bool =
            r >= 0 && c >= 0 && r < dim && c < dim

        [(r-1,c); (r+1,c); (r,c-1); (r,c+1)]
        |> List.filter(isCoordInField dim)


    /// uses the getSurrounding to find the coordinates 
    /// of the empty cell if it is in the surroundings
    /// of the given coordinate - if not the function
    /// will return None - indicating that there is 
    /// no adjacent empty space to move to
    let emptyTileFrom (state : PuzzleState) ((r,c) : Coordinate, (r',c') : Coordinate) : Coordinate option =
        let dim = dim state
        getSurrounding dim (r,c)
        |> List.tryFind (fun coord -> coord = emptyCoord state)

    /// switches two tiles by their coordinates
    /// please note, one of the tiles switched has to be the empty tile,
    /// otherwise switch is not possible.
    let switchTiles (state : PuzzleState) ((r,c) : Coordinate, (r',c') : Coordinate) : PuzzleState =
        let dim = dim state
        let switch = function
            | (x,y) when x = r && y = c -> (r',c')
            | (x,y) when x = r' && y = c' -> (r,c)
            | _ as c -> c

        Array.init dim (fun x -> Array.init dim (fun y -> (x,y) |> switch |> getTile state))



    let moveTileAt (state : PuzzleState) ((r,c) : Coordinate) : PuzzleState =
        match emptyTileFrom state (r,c) with
        | None  -> state
        | Some (r', c') -> switchTiles state ((r,c),(r',c'))