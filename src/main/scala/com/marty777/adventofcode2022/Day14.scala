package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day14Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day14Definitions {
	enum Comparison: 
		case LT, GT, EQ
	case class Coord(x:Int, y:Int)
	case class Grid(rocks:collection.mutable.Map[Coord, Boolean], minCoord:Coord, maxCoord:Coord)
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
		var maxY = grid.maxCoord.y
		var grains = 0
		while(!abyss && !blockage) {
			grains += 1
			var sandCoord:Coord = sandStart
			var moved = true
			var grainDone = false
			while(moved) {
				// comes to rest on the base floor
				if(!withAbyss && sandCoord.y == maxY + 1) {
					moved = false
				}
				// falling
				else if(!gridNotEmpty(grid.rocks, Coord(sandCoord.x, sandCoord.y + 1)) && !gridNotEmpty(sand,Coord(sandCoord.x, sandCoord.y + 1))) {
					moved = true
					sandCoord = Coord(sandCoord.x, sandCoord.y + 1)
				}
				// rolling left
				else if(!gridNotEmpty(grid.rocks, Coord(sandCoord.x-1, sandCoord.y + 1)) && !gridNotEmpty(sand,Coord(sandCoord.x-1, sandCoord.y + 1))) {
					moved = true
					sandCoord = Coord(sandCoord.x-1, sandCoord.y + 1)
				}
				// rolling right
				else if(!gridNotEmpty(grid.rocks, Coord(sandCoord.x+1, sandCoord.y + 1)) && !gridNotEmpty(sand,Coord(sandCoord.x+1, sandCoord.y + 1))) {
					moved = true
					sandCoord = Coord(sandCoord.x+1, sandCoord.y + 1)
				}
				// comes to rest
				else {
					moved = false
				}
				// heading to the abyss.stop updating
				if(sandCoord.y >= maxY+2) {
					moved = false
				}
			}
			
			if(sandCoord.y >= maxY+2) {
				abyss = true
				// the current grain won't come to rest
				grains -= 1
			}
			else if(sandCoord == sandStart) {
				blockage = true
			}
			else {
				sand(Coord(sandCoord.x, sandCoord.y)) = true
			}
		}
		grains
	}
	
	def gridNotEmpty(grid: collection.mutable.Map[Coord, Boolean], coord:Coord): Boolean = {
		if(!grid.contains(coord)) false 
		else grid(coord)
	}
	
	def parseLines(lines:Seq[String]) = {
		val grid1:collection.mutable.Map[Coord, Boolean] = collection.mutable.Map()
		var minX = 0
		var minY = 0
		var maxX = 0
		var maxY = 0
		for(line <- lines) {
			var points = line.split(" -> ").map( str => Coord(str.split(",")(0).toInt, str.split(",")(1).toInt) )
			for(i <- 0 until points.size - 1) {
				val p1 = points(i)
				val p2 = points(i + 1)
				if(p1.x == p2.x) {
					if(p1.y < p2.y) {
						for(y <- p1.y to p2.y) {
							grid1(Coord(p1.x, y)) = true
						}
					}
					else {
						for(y <- p2.y to p1.y) {
							grid1(Coord(p1.x, y)) = true
						}
					}
				}
				else {
					if(p1.x < p2.x) {
						for(x <- p1.x to p2.x) {
							grid1(Coord(x,p1.y)) = true
						}
					}
					else {
						for(x <- p2.x to p1.x) {
							grid1(Coord(x, p1.y)) = true
						}
					}
				}
			}
		}
		minX = grid1.keys.toSeq.sortWith(_.x < _.x).toSeq.head.x
		maxX = grid1.keys.toSeq.sortWith(_.x > _.x).toSeq.head.x
		minY = grid1.keys.toSeq.sortWith(_.y < _.y).toSeq.head.y
		maxY = grid1.keys.toSeq.sortWith(_.y > _.y).toSeq.head.y
		Grid(grid1, Coord(minX, minY), Coord(maxX, maxY))
	}	
}

