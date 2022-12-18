package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day18Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day18Definitions {
	case class Coord(x:Int, y:Int, z:Int)
}

object Day18 extends PuzzleDay[Seq[Coord], Seq[Coord], Int, Int] {
	override def parse1(inputPath: String): Seq[Coord] = readLines(inputPath).flatMap(parseCubes)
	override def parse2(inputPath: String): Seq[Coord] = parse1(inputPath)

	override def part1(cubes: Seq[Coord]): Int =  {
		var sidesum = 0
		val moves = Seq(Coord(1,0,0),Coord(-1,0,0),Coord(0,1,0),Coord(0,-1,0),Coord(0,0,1),Coord(0,0,-1))
		for(cube <- cubes) {	
			for(move <- moves) {
				val testCoord = coordAdd(cube, move)
				if(!cubes.contains(testCoord)) {
					sidesum += 1
				}
			}
		}
		sidesum
	}
	
	override def part2(cubes: Seq[Coord]): Int =  {
		explore(cubes)
	}
	
	def explore(cubes: Seq[Coord]): Int = {
		val explored:scala.collection.mutable.Map[Coord, Int] = scala.collection.mutable.Map()
		val stack:scala.collection.mutable.Stack[Coord] = scala.collection.mutable.Stack()
		
		val moves = Seq(Coord(1,0,0),Coord(-1,0,0),Coord(0,1,0),Coord(0,-1,0),Coord(0,0,1),Coord(0,0,-1))
		
		// bounds of our search space
		val minX = cubes.sortWith(_.x < _.x).head.x - 2
		val maxX = cubes.sortWith(_.x > _.x).head.x + 2
		val minY = cubes.sortWith(_.y < _.y).head.y - 2
		val maxY = cubes.sortWith(_.y > _.y).head.y + 2
		val minZ = cubes.sortWith(_.z < _.z).head.z - 2
		val maxZ = cubes.sortWith(_.z > _.z).head.z + 2
		
		val minCoord = Coord(minX, minY, minZ)
		val maxCoord = Coord(maxX, maxY, maxZ)
		
		// a starting point safely exterior to any cube structures
		val start = Coord(maxX, maxX, maxZ)
		stack.push(start)
		
		while(stack.size > 0) {
			val coord = stack.pop()
			var sidesum = 0
			for(move <- moves) {
				val testCoord = coordAdd(coord, move)
				if(cubes.contains(testCoord)) {
					sidesum += 1
				}
				else {
					if(withinBounds(minCoord, maxCoord, testCoord) 
						&& !stackOrExploredContains(explored,stack, testCoord)) {
						stack.push(testCoord)
					}
				}
			}	
			explored(coord) = sidesum
		}
		explored.toSeq.map(_._2).sum
	}
	
	def coordAdd(coord1:Coord, coord2:Coord): Coord = {
		Coord(coord1.x + coord2.x, coord1.y + coord2.y, coord1.z + coord2.z)
	}
	
	def withinBounds(min:Coord, max:Coord, coord:Coord): Boolean = {
		coord.x >= min.x && coord.x <= max.x &&
		coord.y >= min.y && coord.y <= max.y &&
		coord.z >= min.z && coord.z <= max.z
		
	}
	
	
	def stackOrExploredContains(explored: scala.collection.mutable.Map[Coord, Int], stack:scala.collection.mutable.Stack[Coord], coord:Coord):Boolean = {
		explored.contains(coord) || stack.contains(coord)
	}
	
	def parseCubes(line: String): Option[Coord] = {
		val cube = "(.*),(.*),(.*)".r
		line match {
			case cube(x,y,z) => Some(Coord(x.toInt, y.toInt, z.toInt))
			case _ => None
		}
	}
}

