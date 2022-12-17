package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day17Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day17Definitions {
	case class Coord(x:Int, y:Int)
	case class RockSample(wind:Int, steps:Int, rockHeight:Int, dropped:Int)
}

object Day17 extends PuzzleDay[Seq[String], Seq[String], Long, Long] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)

	override def part1(lines: Seq[String]): Long =  simulate(lines(0), 2022, part2 = false)
	override def part2(lines: Seq[String]): Long =  simulate(lines(0), 1000000000000, part2 = true)
	
	def simulate(sequence:String, stop:Long, part2:Boolean = false): Long = {
		val wellWidth = 7
		// pre-parsed shapes
		val shapes = List(List(Coord(0,0), Coord(1,0), Coord(2,0), Coord(3,0)), List(Coord(1,2), Coord(0,1), Coord(1,1), Coord(2,1), Coord(1,0)), List(Coord(2,2), Coord(2,1), Coord(0,0), Coord(1,0), Coord(2,0)), List(Coord(0,3), Coord(0,2), Coord(0,1), Coord(0,0)), List(Coord(0,1), Coord(1,1), Coord(0,0), Coord(1,0)))
		
		var rocks:collection.mutable.Map[Coord, Boolean] = collection.mutable.Map()
		var dropped = 0
		var wind = 0
		var shape = 0
		var steps = 0
		var done = false
		var rockSamples:Seq[RockSample] = Seq()
		var part2Result:Long = 0
		while(!done) {
			var startY = 0
			var maxY = -1 // my y coords got confused somewhere, but this works
			if(rocks.size > 0) {
				maxY = rocks.keys.toSeq.sortWith(_.y > _.y).head.y
			}
			var positionOffset = Coord(2, maxY + 4)
			var rest = false
			while(!rest) {
				// blow stage
				val windOffset = if(sequence(wind) == '<') then -1 else 1
				wind = (wind + 1) % sequence.length
				// speculative move
				var speculativeCoords = shapes(shape).map(c => Coord(c.x + positionOffset.x + windOffset, c.y + positionOffset.y))
				if(!overlap(rocks, speculativeCoords) 
					&& speculativeCoords.sortWith(_.x < _.x).head.x >= 0 
					&& speculativeCoords.sortWith(_.x > _.x).head.x < wellWidth) {
					positionOffset = Coord(positionOffset.x + windOffset, positionOffset.y)
				}
				// fall stage
				speculativeCoords = shapes(shape).map( c => Coord(c.x + positionOffset.x, c.y + positionOffset.y - 1))
				if(!overlap(rocks, speculativeCoords) && speculativeCoords.sortWith(_.y < _.y).head.y >= 0) {
					positionOffset = Coord(positionOffset.x, positionOffset.y - 1)
				}
				else {
					rest = true
				}
				steps += 1
			}
			// apply the resting shape to the rocks
			for(i <- 0 until shapes(shape).size) {
				rocks(Coord(shapes(shape)(i).x + positionOffset.x, shapes(shape)(i).y + positionOffset.y)) = true
			}
			// update shape index
			shape = (shape + 1) % shapes.size
			dropped += 1
			// stopping condition
			if(part2) {
				// analysis for repeating sequences
				rockSamples = rockSamples :+ RockSample(wind, steps, rocks.keys.toSeq.sortWith(_.y > _.y).head.y + 1, dropped)
				// find two previous samples with the same wind value as current. 
				// If the rockHeights, dropped rocks and steps differ by a 
				// constant amount, we've found a cycle.
				val prevSamples = rockSamples.filter(rs => rs.wind == wind)
				if(prevSamples.size > 2) {
					val droppedRocksDiff1 = prevSamples(prevSamples.size - 1).dropped -  prevSamples(prevSamples.size - 2).dropped
					val droppedRocksDiff2 = prevSamples(prevSamples.size - 2).dropped -  prevSamples(prevSamples.size - 3).dropped
					val stepsDiff1 = prevSamples(prevSamples.size - 1).steps - prevSamples(prevSamples.size - 2).steps 
					val stepsDiff2 = prevSamples(prevSamples.size - 2).steps - prevSamples(prevSamples.size - 3).steps 
					val rockHeightDiff1 = prevSamples(prevSamples.size - 1).rockHeight - prevSamples(prevSamples.size - 2).rockHeight 
					val rockHeightDiff2 = prevSamples(prevSamples.size - 2).rockHeight - prevSamples(prevSamples.size - 3).rockHeight 
					if(droppedRocksDiff1 == droppedRocksDiff2 
						&& stepsDiff1 == stepsDiff2 
						&& rockHeightDiff1 == rockHeightDiff2)
					{
						// If we're in a cycle, we've already recorded a rock 
						// sample that can tell us 
						// (some number of cycles)*cycleHeight + the sample height
						// that gives the height at the required number of dropped rocks
						val cycleOffsetDropped = prevSamples(0).dropped
						val cycleDropped = droppedRocksDiff1
						val cycleHeight = rockHeightDiff1
						
						val rockIndex = (stop - cycleOffsetDropped) % cycleDropped + cycleOffsetDropped - 1 // index is 0 based, dropped rocks is 1 based
						val theRockSample = rockSamples(rockIndex.toInt)
						val repeats = (stop-cycleOffsetDropped)/cycleDropped
						part2Result = repeats * cycleHeight + theRockSample.rockHeight
						done = true
					}
				}
			}
			else {
				if(dropped == stop) then done = true
			}
		}
		if(part2) then part2Result
		else rocks.keys.toSeq.sortWith(_.y > _.y).head.y + 1 // maximum y value in fallen rocks, +1 because my indexing is off
	}
	
	def overlap(rocks: collection.mutable.Map[Coord, Boolean], symbol:Seq[Coord]): Boolean = {
		var i = 0
		var collision = false
		while(i < symbol.size && !collision) {
			if(rocks.contains(symbol(i)) && rocks(symbol(i)) == true) {
				collision = true
			}
			i+=1
		}
		collision
	}
}

