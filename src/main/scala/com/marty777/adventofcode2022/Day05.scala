package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day05Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day05Definitions {
	case class Instruction(amount:Int, source:Int, destination:Int)
	// wrapper class to avoid having to continually specify mutable map type
	case class StackCollection(stacks:scala.collection.mutable.Map[Int,Seq[String]])
}

object Day05 extends PuzzleDay[Seq[String], Seq[String], String, String] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)
	
	override def part1(lines: Seq[String]): String = {
		val (stackCollection, instructions) = parseLines(lines)
		for(instruction <- instructions) {
			runInstruction(stackCollection, instruction, part1=true)
		}
		topCrates(stackCollection)
	}
	override def part2(lines: Seq[String]): String = {
		val (stackCollection, instructions) = parseLines(lines)
		for(instruction <- instructions) {
			runInstruction(stackCollection, instruction, part1=false)
		}
		topCrates(stackCollection)
	}
	
	def topCrates(stackCollection:StackCollection): String = {
		var result = ""
		for(k <- stackCollection.stacks.keys.toSeq.sorted) {
			result = result + stackCollection.stacks(k)(0)
		}
		result
	}
	
	def runInstruction(stackCollection:StackCollection, instruction:Instruction, part1:Boolean): Unit = {
		if(part1) {
			for(i <- 1 to instruction.amount) {
				val crate = popStack(stackCollection, instruction.source, 1)
				pushStack(stackCollection, instruction.destination, crate)
			}
		}
		else {
			val crates = popStack(stackCollection, instruction.source, instruction.amount)
			pushStack(stackCollection, instruction.destination, crates)
		}
	}
	
	def pushStack(stackCollection:StackCollection, stackIndex: Int, pushItems: Seq[String]): Unit = {
		if(!stackCollection.stacks.contains(stackIndex)) {
			stackCollection.stacks.update(stackIndex, Seq())
		}
		stackCollection.stacks.update(stackIndex, pushItems ++ stackCollection.stacks(stackIndex))
	}
	
	def popStack(stackCollection:StackCollection, stackIndex: Int, amount: Int): Seq[String] = {
		val popped = stackCollection.stacks(stackIndex).take(amount)
		stackCollection.stacks.update(stackIndex, stackCollection.stacks(stackIndex).takeRight(stackCollection.stacks(stackIndex).length - amount))
		popped.toSeq
	}
	
	def parseLines(lines: Seq[String]): (StackCollection, Seq[Instruction]) = {
		val separator = lines.indexOf("")
		var stackCollection = parseStacks(lines.take(separator))
		val instructions = lines.takeRight(lines.size - separator).flatMap(parseInstruction)
		(stackCollection, instructions)
	}
	
	def parseStacks(lines: Seq[String]): StackCollection = {
		var stackCollection:StackCollection = StackCollection(scala.collection.mutable.Map())
		for(i <- 0 until lines.size) {
			val values = "[A-Z]".r
			val found = values.findAllMatchIn(lines(i)).map( x=> (x.toString, x.start)).toSeq
			for(j <- found) {
				pushStack(stackCollection, j._2/4 + 1, Seq(j._1))
			}
		}
		// each stack was added in reverse order, so fix that
		for((k,v) <- stackCollection.stacks) {
			stackCollection.stacks.update(k, v.reverse)
		}
		stackCollection
	}
	
	def parseInstruction(input: String): Option[Instruction] = {
		val instruction = "move ([0-9]*) from ([0-9]*) to ([0-9]*)".r
		input match {
			case instruction(amount, source, destination)	=> Some(Instruction(amount.toInt, source.toInt, destination.toInt))
			case _	=> None
		}
	}
	
}