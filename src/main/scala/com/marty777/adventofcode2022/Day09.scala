package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day09Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day09Definitions {
	case class Coord(x:Int,y:Int)
	case class Instruction(dir:String, amount:Int)
}

object Day09 extends PuzzleDay[Seq[Instruction], Seq[Instruction], Int, Int] {
	override def parse1(inputPath: String): Seq[Instruction] = readLines(inputPath).flatMap(parseLine)
	override def parse2(inputPath: String): Seq[Instruction] = parse1(inputPath)
	
	override def part1(instructions: Seq[Instruction]): Int = {
		ropeInstructions(instructions, 2)
	}
	
	override def part2(instructions: Seq[Instruction]): Int = {
		ropeInstructions(instructions, 10)
	}
	
	def ropeInstructions(instructions: Seq[Instruction], ropeSegments: Int): Int = {
		var positions:Set[Coord] = Set()
		var headCoord:Coord = Coord(0,0)
		var segments:scala.collection.mutable.Seq[Coord] = scala.collection.mutable.Seq()
		// must be a better way to do this
		for(i <- 0 until ropeSegments) {
			segments =  segments :+ Coord(0,0)
		}
		for(instruction <- instructions) {
			var amount = instruction.amount
			var dir = instruction.dir
			while(amount > 0) {
				dir match  {
					case "U" => {
						headCoord = coordAdd(headCoord, 0, 1)
					}
					case "R" => {
						headCoord = coordAdd(headCoord, 1, 0)
					}
					case "D" => {
						headCoord = coordAdd(headCoord, 0, -1)
					}
					case "L" => {
						headCoord = coordAdd(headCoord, -1, 0)
					}
				}
				segments.update(0, headCoord)
				for(segment <- 1 until segments.size) {
					val nextCoord = updateRope(segments(segment - 1), segments(segment))
					segments.update(segment, nextCoord)
				}
				positions += segments.last
				amount -= 1
			}
		}
		positions.size
	}
	
	def updateRope(parentCoord:Coord, childCoord:Coord):Coord = {
		if((parentCoord.x - childCoord.x).abs < 2 && (parentCoord.y - childCoord.y).abs < 2) {
			childCoord
		}
		else {
			coordAdd(childCoord, sign(parentCoord.x - childCoord.x), sign(parentCoord.y - childCoord.y))
		}
	}
	
	def coordAdd(coord:Coord, dX:Int, dY:Int): Coord = {
		Coord(coord.x + dX, coord.y + dY)
	}
	
	def sign(x:Int):Int = {
		x match {
			case i if i < 0 => -1
			case 0 => 0
			case i if i > 0 => 1
		}	
	}
	
	def parseLine(line: String): Option[Instruction] = {
		val instruction= "(.*) (.*)".r
		line match {
			case instruction(dir, amt) => Some(Instruction(dir, amt.toInt))
			case _ => None
		}
	}
}
