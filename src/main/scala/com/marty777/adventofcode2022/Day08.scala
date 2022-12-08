package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day08Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day08Definitions {
	enum Direction:
		case North, East, South, West
}

object Day08 extends PuzzleDay[Seq[Seq[Int]], Seq[Seq[Int]], Int, Int] {
	override def parse1(inputPath: String): Seq[Seq[Int]] = parseLines(readLines(inputPath))
	override def parse2(inputPath: String): Seq[Seq[Int]] = parse1(inputPath)
	
	override def part1(grid: Seq[Seq[Int]]): Int = {
		Direction.values.foldLeft(Set():Set[(Int, Int)])(_ union visibleTrees(grid, _)).size
		
	}
	override def part2(grid: Seq[Seq[Int]]): Int = {
		var treeScoreMax = 0
		for {
			x <- 0 until grid.size
			y <- 0 until grid.size
		} {
			val treeScore =  Direction.values.foldLeft(1)(_ * visibleTreesFromTreehouse(grid, _, x, y))
			if(treeScore > treeScoreMax) {
				treeScoreMax = treeScore
			}
		}
		treeScoreMax
	}
	
	def visibleTreesFromTreehouse(grid: Seq[Seq[Int]], dir: Direction, startX:Int, startY:Int) : Int = {
		var count = 0
		var blocked = false
		dir match {
			case Direction.North => {
				var y = startY - 1
				while(y >= 0 && !blocked) {
					count += 1
					if(grid(startX)(y) >= grid(startX)(startY)) {
						blocked = true
					}
					y -= 1
				}
			}
			case Direction.East => {
				var x = startX + 1
				while(x < grid.size && !blocked) {
					count += 1
					if(grid(x)(startY) >= grid(startX)(startY)) {
						blocked = true
					}
					x += 1
				}
			}
			case Direction.South => {
				var y = startY + 1
				while(y < grid.size && !blocked) {
					count += 1
					if(grid(startX)(y) >= grid(startX)(startY)) {
						blocked = true
					}
					y += 1
				}
			}
			case Direction.West => {
				var x = startX - 1
				while(x >= 0 && !blocked) {
					count += 1
					if(grid(x)(startY) >= grid(startX)(startY)) {
						blocked = true
					}
					x -= 1
				}
			}
		}
		count
	}
	
	def visibleTrees(grid: Seq[Seq[Int]], dir:Direction): Set[(Int, Int)] = {
		var coords:Set[(Int, Int)] = Set()
		dir match {
			case Direction.North	=> {
				for(x <- 0 until grid.size) {
					var curr = -1
					for(y <- 0 until grid.size) {
						if(curr < grid(x)(y)) {
							curr = grid(x)(y)
							coords += (x,y)
						}
					}
				}
			}
			case Direction.East	=> {
				for(y <- 0 until grid.size) {
					var curr = -1
					for(x <- grid.size - 1 to 0 by -1) {
						if(curr < grid(x)(y)) {
							curr = grid(x)(y)
							coords += (x,y)
						}
					}
				}
			}
			case Direction.South	=> {
				for(x <- 0 until grid.size) {
					var curr = -1
					for(y <- grid.size - 1 to 0 by -1) {
						if(curr < grid(x)(y)) {
							curr = grid(x)(y)
							coords += (x,y)
						}
					}
				}
			}
			case Direction.West	=> {
				for(y <- 0 until grid.size) {
					var curr = -1
					for(x <- 0 until grid.size) {
						if(curr < grid(x)(y)) {
							curr = grid(x)(y)
							coords += (x,y)
						}
					}
				}
			}
		}
		coords
	}
	
	def parseLines(lines:Seq[String]): Seq[Seq[Int]] = {
		var dim = lines.size
		var grid:Seq[Seq[Int]] = Seq()
		for(line <- lines) {
			grid = grid :+ line.toCharArray.map(_.toString.toInt)
		}
		grid
	}
}
