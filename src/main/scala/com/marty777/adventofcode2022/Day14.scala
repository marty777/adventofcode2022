package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day14Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day14Definitions {
	case class Coord(x:Int, y:Int)
	case class Grid(rocks:collection.mutable.Map[Coord, Boolean], maxY:Int)
}

object Day14 extends PuzzleDay[Grid, Grid, Int, Int] {
	override def parse1(inputPath: String): Grid = parseLines(readLines(inputPath))
	override def parse2(inputPath: String): Grid = parse1(inputPath)
	
	override def part1(grid: Grid): Int = simulate(grid, withAbyss = true)
	override def part2(grid: Grid): Int = simulate(grid, withAbyss = false)
	
	def simulate(grid:Grid, withAbyss:Boolean):Int = {
		val sandStart = Coord(500, 0)
		val sand:collection.mutable.Map[Coord, Boolean] = collection.mutable.Map()
		var abyss = false
		var blockage = false
		val abyssY = grid.maxY + 2
		val baseY = grid.maxY + 1 // the maximum depth a grain can reach if there's a floor.
		var grains = 0
		var downCoord = Coord(0,0)
		var leftCoord = Coord(0,0)
		var rightCoord = Coord(0,0)
		while(!abyss && !blockage) {
			grains += 1
			var sandCoord:Coord = sandStart
			var moved = true
			var grainDone = false
			while(moved) {
				downCoord = Coord(sandCoord.x, sandCoord.y + 1)
				leftCoord = Coord(sandCoord.x-1, sandCoord.y + 1)
				rightCoord = Coord(sandCoord.x+1, sandCoord.y + 1)
				// comes to rest on the base floor
				if(!withAbyss && sandCoord.y == baseY) {
					moved = false
				}
				// falling
				else if(!gridNotEmpty(grid.rocks, downCoord) && !gridNotEmpty(sand, downCoord)) {
					moved = true
					sandCoord = downCoord
				}
				// rolling left
				else if(!gridNotEmpty(grid.rocks, leftCoord) && !gridNotEmpty(sand, leftCoord)) {
					moved = true
					sandCoord = leftCoord
				}
				// rolling right
				else if(!gridNotEmpty(grid.rocks, rightCoord) && !gridNotEmpty(sand, rightCoord)) {
					moved = true
					sandCoord = rightCoord
				}
				// comes to rest
				else {
					moved = false
				}
				// heading to the abyss. stop updating
				if(sandCoord.y == abyssY) {
					moved = false
				}
			}
			
			if(sandCoord.y == abyssY) {
				abyss = true
				// the current grain won't come to rest
				grains -= 1
			}
			else if(sandCoord == sandStart) {
				blockage = true
			}
			else {
				sand(sandCoord) = true
			}
		}
		grains
	}
	
	def gridNotEmpty(grid: collection.mutable.Map[Coord, Boolean], coord:Coord): Boolean = {
		if(!grid.contains(coord)) false 
		else grid(coord)
	}
	
	def parseLines(lines:Seq[String]) = {
		val rocks:collection.mutable.Map[Coord, Boolean] = collection.mutable.Map()
		var maxY = 0
		for(line <- lines) {
			var points = line.split(" -> ").map( str => Coord(str.split(",")(0).toInt, str.split(",")(1).toInt) )
			for(i <- 0 until points.size - 1) {
				val p1 = points(i)
				val p2 = points(i + 1)
				if(p1.x == p2.x) {
					if(p1.y < p2.y) {
						for(y <- p1.y to p2.y) {
							rocks(Coord(p1.x, y)) = true
						}
					}
					else {
						for(y <- p2.y to p1.y) {
							rocks(Coord(p1.x, y)) = true
						}
					}
				}
				else {
					if(p1.x < p2.x) {
						for(x <- p1.x to p2.x) {
							rocks(Coord(x,p1.y)) = true
						}
					}
					else {
						for(x <- p2.x to p1.x) {
							rocks(Coord(x, p1.y)) = true
						}
					}
				}
			}
		}
		maxY = rocks.keys.toSeq.sortWith(_.y > _.y).toSeq.head.y
		// the rocks map no longer needs to be updated and should probably
		// be converted to an immutable map, but eh.
		Grid(rocks, maxY)
	}	
}

