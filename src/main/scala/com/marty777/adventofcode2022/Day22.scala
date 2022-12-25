package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day22Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day22Definitions {
	case class Coord(x:Int, y:Int)
	case class Coord3(var x:Int, var y:Int, var  z:Int)
	//case class MapNode(position:Coord, isObstacle:Boolean)
	case class Instruction(amt:Int, isDir:Boolean, dir:String)
	case class State(grid: collection.mutable.Map[Coord, Boolean], instructions:Seq[Instruction], width:Int, height:Int)
	// faceX, faceY are the directions of the x and y vectors in input space (i.e. left to right and up to down) mapped into 3d coordinates
	// when on the cube.
	case class CubeFace(normal: Coord3, inputSpaceStart:Coord, faceX:Coord3, faceY:Coord3)
	case class Cube(faces: Seq[CubeFace], dim:Int, faceCoordLookup:Map[(Coord3, Int), Coord])
}

object Day22 extends PuzzleDay[Seq[String], Seq[String], Long, Long] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)

	override def part1(lines: Seq[String]): Long =  {
		val state = parseLines(lines)
		turtle2D(state)
	}
	
	// note that part 2 may not be a general solution. See the parseCube method
	override def part2(lines: Seq[String]): Long =  {
		val state = parseLines(lines)
		val cube = parseCube(state)
		turtle3D(state, cube)
	}
	
	// Hardcoding my input face mapping and the example face mapping.
	// If the cube layout differs, this solution may not work with all puzzle inputs
	// Discussion: Dealing with 3 coordinate systems: input coordinates, face coordinates (going 0 to dim - 1 on each face)
	// and 3d coordinates. Each face includes a normal in 3d coordinates, a coordinate corresponding to the top-left position 
	// of the face in input coordinates, and the 3d vectors corresponding to the x and y directions of face/input space mapped 
	// onto 3d space after folding the cube. The implementation could probably simplify out the face/input space into a single
	// coordinate system, but I'm not touching this any further.
	def parseCube(state:State): Cube = {
		var cubeFaces = Seq.empty[CubeFace]
		var dim = 0;
		// the example
		if(state.grid.size == 96) {
			dim = 4
			// starting face "1"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(0,0,-1), Coord(8, 0), Coord3(1,0,0), Coord3(0,1,0))
			// "2"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(0,-1,0), Coord(0, 4), Coord3(-1,0,0), Coord3(0,0,1))
			// "3"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(-1,0,0), Coord(4, 4), Coord3(0,1,0), Coord3(0,0,1))
			// "4"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(0,1,0), Coord(8, 4), Coord3(1,0,0), Coord3(0,0,1))
			// "5"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(0,0,1), Coord(8, 8), Coord3(1,0,0), Coord3(0,-1,0))
			// "6"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(1,0,0), Coord(12, 8), Coord3(0,0,-1), Coord3(0,-1,0))
			
		}
		// the input
		else {
			dim = 50
			// starting face "1"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(0,0,-1), Coord(50, 0), Coord3(1,0,0), Coord3(0,1,0))
			// "2"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(1,0,0), Coord(100, 0), Coord3(0,0,1), Coord3(0,1,0))
			// "3"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(0,1,0), Coord(50, 50), Coord3(1,0,0), Coord3(0,0,1))
			// "5"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(0,0,1), Coord(50, 100), Coord3(1,0,0), Coord3(0,-1,0))
			// "4"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(-1,0,0), Coord(0, 100), Coord3(0,0,1), Coord3(0,-1,0))
			// "6"
			cubeFaces = cubeFaces :+ CubeFace(Coord3(0,-1,0), Coord(0, 150), Coord3(0,0,1), Coord3(1,0,0))
		}
		// forget doing change of basis math, we can just cache a lookup of all cube-space coordinates on a face to equivalent face-space coords
		val faceCoordMap = collection.mutable.Map.empty[(Coord3, Int), Coord]
		val dimMax = dim - 1
		for(f <- 0 until cubeFaces.size) {
			for(y <- 0 until dim) {
				for(x <- 0 until dim) {
					var cubeCoord = Coord3(0,0,0)
					// origin component from normal
					if(cubeFaces(f).normal.x > 0) {
						cubeCoord.x = dimMax
					}
					else if(cubeFaces(f).normal.y > 0) {
						cubeCoord.y = dimMax
					}
					else if(cubeFaces(f).normal.z > 0) {
						cubeCoord.z = dimMax
					}
					// origin component from faceX, faceY vectors
					if(cubeFaces(f).faceX.x == -1 || cubeFaces(f).faceY.x == -1) {
						cubeCoord.x = dimMax
					}
					if(cubeFaces(f).faceX.y == -1 || cubeFaces(f).faceY.y == -1) {
						cubeCoord.y = dimMax
					}
					if(cubeFaces(f).faceX.z == -1 || cubeFaces(f).faceY.z == -1) {
						cubeCoord.z = dimMax
					}
					// add faceX, faceY vectors in x and y magnitude
					cubeCoord = Coord3Add(cubeCoord, Coord3ScalarMult(cubeFaces(f).faceX, x))
					cubeCoord = Coord3Add(cubeCoord, Coord3ScalarMult(cubeFaces(f).faceY, y))
					faceCoordMap((cubeCoord, f)) = Coord(x,y)
				}
			}
		}
		Cube(cubeFaces, dim, faceCoordMap.toMap)
	}
	
	def Coord3Add(a:Coord3, b:Coord3):Coord3 = {
		Coord3(a.x + b.x, a.y + b.y, a.z + b.z)
	}
	
	def Coord3ScalarMult(a:Coord3, s:Int):Coord3 = {
		Coord3(a.x * s, a.y * s, a.z * s)
	}
	
	// the index of the cube face corresponding to the input-space coordinate
	def inputPosToFace(pos:Coord, cube:Cube):Int = {
		cube.faces.zipWithIndex.filter((c,i) => pos.x >= c.inputSpaceStart.x && pos.x < c.inputSpaceStart.x + cube.dim && pos.y >= c.inputSpaceStart.y && pos.y < c.inputSpaceStart.y + cube.dim).head._2
	}
	
	def inputPosToFacePos(pos:Coord, cube:Cube):Coord = {
		val faceIndex = inputPosToFace(pos, cube)
		Coord(pos.x - cube.faces(faceIndex).inputSpaceStart.x, pos.y - cube.faces(faceIndex).inputSpaceStart.y)
	}
	
	def facePosToInputPos(facePos:Coord, faceIndex:Int, cube:Cube): Coord = {
		Coord(facePos.x + cube.faces(faceIndex).inputSpaceStart.x, facePos.y + cube.faces(faceIndex).inputSpaceStart.y)
	}
	
	// def mapToDim(value:Int, dimMax:Int):Int = {
		// if(value != 1 && value != -1) {
			// throw Exception(s"Incorrect usage of mapToDim with value $value")
		// }
		// if(value == -1) then 0 else dimMax
	// }
	
	def facePosToCubeCoord(cube:Cube, faceIndex:Int, facePos:Coord):Coord3 = {
		var total = Coord3(0,0,0)
		val dimMax = cube.dim - 1
		// origin in the normal direction
		if(cube.faces(faceIndex).normal.x > 0) {
			total.x = dimMax
		}
		else if(cube.faces(faceIndex).normal.y > 0) {
			total.y = dimMax
		}
		else if(cube.faces(faceIndex).normal.z > 0) {
			total.z = dimMax
		}
		// origin in the faceX direction 
		if(cube.faces(faceIndex).faceX.x < 0 || cube.faces(faceIndex).faceX.y < 0 || cube.faces(faceIndex).faceX.z < 0 ) {
			total = Coord3Add(Coord3ScalarMult(cube.faces(faceIndex).faceX, -dimMax), total)
		}
		// origin in the faceY direction 
		if(cube.faces(faceIndex).faceY.x < 0 || cube.faces(faceIndex).faceY.y < 0 || cube.faces(faceIndex).faceY.z < 0 ) {
			total = Coord3Add(Coord3ScalarMult(cube.faces(faceIndex).faceY, -dimMax), total)
		}
		// add the faceX and faceY vectors multiplied by the x and y magnitudes and return
		total = Coord3Add(total, Coord3ScalarMult(cube.faces(faceIndex).faceX, facePos.x))
		Coord3Add(total, Coord3ScalarMult(cube.faces(faceIndex).faceY, facePos.y))
	}
	
	// checks the lookup for a mapping of a 3d coordinate to a cube face in face coordinates
	def cubeCoordToFacePos(cube:Cube, faceIndex:Int, cubeCoord:Coord3):Coord = {
		cube.faceCoordLookup((cubeCoord, faceIndex))
	}
	
	// returns the next input-space coordinate and input-space direction, or none if an obstacle was encountered.
	def next3D(state:State, cube:Cube, inputPos:Coord, dir:Int, step:Int = 0):Option[(Coord, Int)] = {
		val dimMax = cube.dim - 1
		var found = false
		val startFaceIndex = inputPosToFace(inputPos, cube)
		val facePos = inputPosToFacePos(inputPos, cube)
		var nextFaceX = facePos.x
		var nextFaceY = facePos.y
		var obstacle = false
		dir match {
			case 0 => nextFaceX += 1
			case 1 => nextFaceY += 1
			case 2 => nextFaceX -= 1
			case 3 => nextFaceY -= 1
			case _ => throw Exception("Unexpected direction $dir")
		}
		// move on the current face
		if(nextFaceX >= 0 && nextFaceX < cube.dim && nextFaceY >= 0 && nextFaceY < cube.dim) {
			val nextInputPos = facePosToInputPos(Coord(nextFaceX, nextFaceY), startFaceIndex, cube)
			if(state.grid(nextInputPos) == true) {
				Some(nextInputPos, dir)
			}
			else {
				None
			}
		}
		// move to a new face
		else {
			// determine direction and position on the next face 
			// the direction we've moved off our current face can tell us the normal of the next face
			var nextNormal = Coord3(-1,-1,-1)
			// -x direction
			if(nextFaceX < 0) {
				nextNormal = Coord3ScalarMult(cube.faces(startFaceIndex).faceX, -1)
			}
			// +x direction
			else if(nextFaceX > dimMax) {
				nextNormal = cube.faces(startFaceIndex).faceX
			}
			// -y direction
			else if(nextFaceY < 0) {
				nextNormal = Coord3ScalarMult(cube.faces(startFaceIndex).faceY, -1)
				
			}
			// +y direction
			else if(nextFaceY > dimMax) {
				nextNormal = cube.faces(startFaceIndex).faceY			
			}
			
			// given the next normal, look up the face index
			var nextFaceIndex = cube.faces.zipWithIndex.filter((f,i) => f.normal == nextNormal).head._2
			
			// next direction in cube space is the opposite direction of the current face's normal
			var nextDirectionCoord3 = Coord3ScalarMult(cube.faces(startFaceIndex).normal, -1)
			// next position in cube space is the same as the current position (we've just wrapped around a side)
			var nextPositionCoord3 = facePosToCubeCoord(cube, startFaceIndex, inputPosToFacePos(inputPos, cube))
			
			// determine the position and direction in the coordinates of the next face
			val nextFacePos = cubeCoordToFacePos(cube, nextFaceIndex, nextPositionCoord3)
			val nextCubeCoordInDirection = Coord3Add(nextPositionCoord3, nextDirectionCoord3)
			val nextFacePosInDirection = cubeCoordToFacePos(cube, nextFaceIndex, nextCubeCoordInDirection)
			
			var nextDir = 0
			if(nextFacePosInDirection.x - nextFacePos.x > 0) {
				nextDir = 0
			}
			else if(nextFacePosInDirection.x - nextFacePos.x < 0) {
				nextDir = 2
			}
			else if(nextFacePosInDirection.y - nextFacePos.y > 0) {
				nextDir = 1
			}
			else if(nextFacePosInDirection.y - nextFacePos.y < 0) {
				nextDir = 3
			}
			val nextInputPos = facePosToInputPos(nextFacePos, nextFaceIndex, cube)
			if(state.grid(nextInputPos) == true) {
				Some((nextInputPos, nextDir))
			}
			else {
				None
			}
		}
	}
	
	def turtle3D(state:State, cube:Cube):Long = {
		var yPos = 0
		var xPos = state.grid.keys.toSeq.filter(c => c.y == 0).sortWith(_. x < _.x).head.x
		var dir = 0
		var instCount = 0
		for(inst <- state.instructions) {
			if(inst.isDir) {
				if(inst.dir == "L") {
					dir = (dir - 1) % 4
					if(dir < 0) {
						dir += 4
					}
				}
				else {
					dir = (dir + 1) % 4
				}
			}
			else {
				var done = false
				var steps = 0
				while(steps < inst.amt && !done) {
					next3D(state, cube, Coord(xPos, yPos), dir, instCount) match {
						case Some(c,d) => {
							xPos = c.x
							yPos = c.y
							dir = d
							steps += 1
						}						
						case None => {
							done = true
						}
					}
				}
			}
			instCount += 1
		}
		((yPos + 1) * 1000) + ((xPos + 1) * 4) + dir
	}
	
	def turtle2D(state:State):Long = {
		var yPos = 0
		var xPos = state.grid.keys.toSeq.filter(c => c.y == 0).sortWith(_. x < _.x).head.x
		var dir = 0
		var instCount = 0
		for(inst <- state.instructions) {
			if(inst.isDir) {
				if(inst.dir == "L") {
					dir = (dir - 1) % 4
					if(dir < 0) {
						dir += 4
					}
				}
				else {
					dir = (dir + 1) % 4
				}
			}
			else {
				var done = false
				var steps = 0
				while(steps < inst.amt && !done) {
					next(state, Coord(xPos, yPos), dir, instCount) match {
						case Some(c) => {
							xPos = c.x
							yPos = c.y
							steps += 1
						}						
						case None => {
							done = true
						}
					}
				}
			}
			instCount += 1
		
		}
		((yPos + 1) * 1000) + ((xPos + 1) * 4) + dir
	}
	
	def next(state:State, coord:Coord, dir:Int, step:Int = 0):Option[Coord] = {
		var found = false
		dir match {
			// right
			case 0 => {
				var x = (coord.x + 1) % state.width
				while(!found && x != coord.x) {
					if(state.grid.contains(Coord(x, coord.y))) {
						found = true
					}
					else {
						x = (x+1) % state.width
					}
				}
				if(x == coord.x) {
					throw Exception("sCouldn't find next $coord right")
				}
				if(found && state.grid(Coord(x, coord.y)) == true ) {
					Some(Coord(x, coord.y))
				}
				else {
					None
				}
			}
			// down
			case 1 => {
				var y = (coord.y + 1) % state.height
				while(!found && y != coord.y) {
					if(state.grid.contains(Coord(coord.x, y))) {
						found = true
					}
					else {
						y = (y+1) % state.height
					}
					
				}
				if(y == coord.y) {
					throw Exception("sCouldn't find next $coord down")
				}
				if(found && state.grid(Coord(coord.x, y)) == true ) {
					Some(Coord(coord.x, y))
				}
				else {
					None
				}
			}
			// left
			case 2 => {
				var x = (coord.x - 1) % state.width
				if(x < 0) {
					x += state.width
				}
				while(!found && x != coord.x) {
					
					if(state.grid.contains(Coord(x, coord.y))) {
						found = true
					}
					else {
						x = (x-1) % state.width
						if(x < 0) {
							x += state.width
						}
					}
				}
				if(x == coord.x) {
					throw Exception("sCouldn't find next $coord left")
				}
				if(found && state.grid(Coord(x, coord.y)) == true ) {
					Some(Coord(x, coord.y))
				}
				else {
					None
				}
			}
			// up
			case 3 => {
				var y = (coord.y - 1) % state.height
				if(y < 0) {
					y += state.height
				}
				while(!found && y != coord.y) {
					if(state.grid.contains(Coord(coord.x, y))) {
						found = true
					}
					else {
						y = (y-1) % state.height
						if(y < 0) {
							y += state.height
						}
					}
				}
				if(y == coord.y) {
					throw Exception("sCouldn't find next $coord up")
				}
				if(found && state.grid(Coord(coord.x, y)) == true ) {
					Some(Coord(coord.x, y))
				}
				else {
					None
				}
			}
		}
	}
	
	def parseLines(lines:Seq[String]) = {
		var doneMap = false
		var index = 0
		val mapNodes = collection.mutable.Map.empty[Coord, Boolean]
		var instructions:Seq[Instruction] = Seq()
		while(!doneMap && index < lines.size) {
			if(lines(index).length == 0) {
				doneMap = true
				index += 1
			}
			else {
				val chars = lines(index).toCharArray
				for(x <- 0 until chars.size) {
					if(chars(x) == '.') {
						mapNodes(Coord(x, index)) = true
					}
					else if(chars(x) == '#') {
						mapNodes(Coord(x, index)) = false
					}
				}
				index += 1
			}
		}
		val instLine = lines(index).toCharArray
		var last = 0
		var curr = 0
		var numBuffer = 0
		while(curr < instLine.size) {
			if(instLine(curr) == 'L' || instLine(curr) == 'R') {
				if(numBuffer > 0) {
					instructions = instructions :+ Instruction(numBuffer, false, "")
					numBuffer = 0
				}
				instructions = instructions :+ Instruction(0, true, instLine(curr).toString)
				
			}
			else {
				numBuffer *= 10
				numBuffer += instLine(curr).toString.toInt
			}
			curr += 1
		}
		if(numBuffer > 0) {
			instructions = instructions :+ Instruction(numBuffer, false, "")
		}
		
		State(mapNodes, instructions, (mapNodes.toSeq.map((k,v) => k.x).max + 1), (mapNodes.toSeq.map((k,v) => k.y).max + 1))
	}
	
}

