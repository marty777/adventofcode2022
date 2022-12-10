package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day10Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day10Definitions {
	case class Instruction(isNoop:Boolean, v:Int)
	case class Processor(var cycles:Int, var x:Int)
}

object Day10 extends PuzzleDay[Seq[Instruction], Seq[Instruction], Int, String] {
	override def parse1(inputPath: String): Seq[Instruction] = readLines(inputPath).flatMap(parseLine)
	override def parse2(inputPath: String): Seq[Instruction] = parse1(inputPath)
	
	override def part1(instructions: Seq[Instruction]): Int = {
		var states = buildStates(Processor(0, 1), instructions, 220)
		(0 to 5).map(20 + _*40).foldLeft(0)(_ + stateVal(states, _))		
	}
	
	override def part2(instructions: Seq[Instruction]): String = {
		drawLines(buildStates(Processor(0, 1), instructions, 240))
	}
	
	// this might be off by one horizontally, but it's legible
	def drawLines(states:Seq[Int]): String = {
		val raster = new StringBuilder("")
		for(i <- 0 until 240) {
			if(i % 40 == 0) {
				raster ++= "\n"
			}
			if(states(i) == (i-1) % 40 || states(i) == i % 40 || states(i) == (i+1) % 40) {
				raster ++= "#"
			}
			else {
				raster ++= "."
			}
		}
		raster.toString
	}
	
	def stateVal(states:Seq[Int], index:Int) : Int = {
		states(index-1) * index
	}
	
	def buildStates(in:Processor, instructions:Seq[Instruction], maxCycles: Int): Seq[Int] = {
		var index:Int = 0
		var builder = Seq.newBuilder[Int]
		while(in.cycles <= maxCycles && index < instructions.size) {
			if(instructions(index).isNoop) {
				builder += in.x
				in.cycles += 1
			}
			else {
				builder += in.x
				builder += in.x
				in.cycles += 2
				in.x += instructions(index).v
			}
			index += 1
		}
		while(in.cycles < maxCycles) {
			builder += in.x
			in.cycles += 1
		}
		builder.result()
	}
	
	def parseLine(line:String): Option[Instruction] = {
		val addx = "addx (.*)".r
		line match {
			case addx(amt) => Some(Instruction(false, amt.toInt))
			case "noop" => Some(Instruction(true, 0))
			case _ => None
		}
	}
}
