package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day20Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day20Definitions {
	case class Node(value:Long, originalIndex:Int)
}

object Day20 extends PuzzleDay[Seq[Long], Seq[Long], Long, Long] {
	override def parse1(inputPath: String): Seq[Long] = readLines(inputPath).map(_.toLong)
	override def parse2(inputPath: String): Seq[Long] = parse1(inputPath)

	override def part1(inputs: Seq[Long]): Long =  {
		val zeroIndex = inputs.indexOf(0)
		val nodes:scala.collection.mutable.ListBuffer[Node] = scala.collection.mutable.ListBuffer.empty[Node]
		for(i <- 0 until inputs.size) {
			nodes += Node(inputs(i),i)
		}
		mix(nodes, inputs)
		groveCoordinates(nodes, zeroIndex)
	}
	
	override def part2(inputs: Seq[Long]): Long =  {
		val key:Long = 811589153
		val zeroIndex = inputs.indexOf(0)
		val decryptedInputs = inputs.map(_ * key)
		val nodes:scala.collection.mutable.ListBuffer[Node] = scala.collection.mutable.ListBuffer.empty[Node]
		for(i <- 0 until decryptedInputs.size) {
			nodes += Node(decryptedInputs(i),i)
		}
		for(i <- 1 to 10) {
			mix(nodes, decryptedInputs)
		}
		groveCoordinates(nodes, zeroIndex)
	}
	
	def mix(nodes:scala.collection.mutable.ListBuffer[Node], inputs:Seq[Long]) = {
		for(i <- 0 until inputs.size) {
			move(nodes, i, inputs(i))
		}
	}
	
	def move(nodes: scala.collection.mutable.ListBuffer[Node], originalIndex:Int, originalValue:Long) = {
		val atIndex = nodes.indexOf(Node(originalValue, originalIndex))
		val v = nodes(atIndex)
		// modulo size - 1 because we're moving over the list after removing one element
		var nextIndex = (atIndex + v.value) % (nodes.size - 1) 
		if(nextIndex < 0) {
			nextIndex += (nodes.size - 1)
		}
		nodes.remove(atIndex)
		nodes.insert(nextIndex.toInt,v)
	}
	
	def groveCoordinates(nodes: scala.collection.mutable.ListBuffer[Node], zeroIndex:Int): Long = {
		valueAt(nodes, 1000, zeroIndex) + valueAt(nodes, 2000, zeroIndex) + valueAt(nodes, 3000, zeroIndex)
	}
	
	def valueAt(nodes: scala.collection.mutable.ListBuffer[Node], index:Int, zeroIndex:Int):Long = {
		val zeroPosition = nodes.indexOf(Node(0, zeroIndex))
		val zeroIndexed = (index + zeroPosition)  % nodes.size
		nodes(zeroIndexed).value
	}
}

