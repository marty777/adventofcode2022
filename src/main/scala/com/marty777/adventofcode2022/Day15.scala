package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day15Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day15Definitions {
	case class Coord(x:Int, y:Int)
	case class Reading(sensor:Coord, beacon:Coord)
	case class Range(start:Int, end:Int)
}

object Day15 extends PuzzleDay[Seq[Reading], Seq[Reading], Long, Long] {
	override def parse1(inputPath: String): Seq[Reading] = readLines(inputPath).flatMap(parseReading)
	override def parse2(inputPath: String): Seq[Reading] = parse1(inputPath)
	
	override def part1(readings: Seq[Reading]): Long = {
		var targetY = 2000000
		// cheap way to test if we're looking at the sample input
		if(readings.size > 0 && readings(0) == Reading(Coord(2,18), Coord(-2, 15))) {
			targetY = 10
		}
		val ranges:Seq[Range] = readings.flatMap(r => readingSlice(r, targetY)).toSeq
		compressRanges(ranges).foldLeft(0:Long)((acc, r) => acc + (r.end - r.start))
	}
	override def part2(readings: Seq[Reading]): Long = {
		val minY = 0
		var maxY = 4000000
		// cheap way to test if we're looking at the sample input
		if(readings.size > 0 && readings(0) == Reading(Coord(2,18), Coord(-2, 15))) {
			maxY = 20
		}
		var y = minY
		var found = false
		var x:Long = 0
		while(!found && y <= maxY) {
			val ranges:Seq[Range] = readings.flatMap(r => readingSlice(r, y)).toSeq
			val compressed:Seq[Range] = compressRanges(ranges)
			if(compressed.size > 1) {
				var gaps = compressed.sortWith(_.start < _.start).toSeq.sliding(2).flatMap(x => rangeGap(x(0),x(1))).toSeq
				if(gaps.size == 1 && gaps(0).start == gaps(0).end) {
					found = true
					x = gaps(0).start
				}
			}
			if(!found) {
				y += 1
			}
		}
		if(!found) {
			throw Exception("Solution could not be found")
		}
		x*4000000 + y
	}
	
	def rangeGap(range1:Range, range2:Range): Option[Range] = {
		if(rangeOverlaps(range1, range2)) {
			None
		}
		else {
			if(range1.start < range2.start) {
				if(range1.end+1 >= range2.start) {
					None
				}
				else {
					Some(Range(range1.end + 1, range2.start - 1))
				}
			}
			else {
				if(range2.end+1 >= range1.start) {
					None
				}
				else {
					Some(Range(range2.end + 1, range1.start - 1))
				}
			}
		}
	}
	
	def compressRanges(ranges:Seq[Range]): Seq[Range] = {
		var done = false
		var startSeq = ranges
		var outSeq:Seq[Range] = Seq()
		while(!done) {
			var compressions = 0
			while(startSeq.size > 0) {
				var theRange = startSeq.head
				startSeq = startSeq.tail
				var j = 0
				while(j < startSeq.size) {
					if(rangeOverlaps(theRange, startSeq(j))) {
						theRange = rangeAdd(theRange, startSeq(j))
						// there has to be a better way to do this
						startSeq = startSeq.zipWithIndex.filter((x,index) => index != j).map((x,index) => x).toSeq
						j -= 1
						compressions += 1
					}
					j += 1
				}
				outSeq = outSeq :+ theRange
			}
			if(compressions != 0) {
				startSeq = outSeq
				outSeq = Seq()
			}
			else {
				done = true
			}
		}
		outSeq
	}
	
	def rangeOverlaps(range1:Range, range2:Range):Boolean = {
		(range1.start >= range2.start && range1.start <= range2.end) || (range1.end >= range2.start && range1.end <= range2.end) ||
		(range2.start >= range1.start && range2.start <= range1.end) || (range2.end >= range1.start && range2.end <= range1.end)
	}
	
	def rangeAdd(range1:Range, range2:Range):Range = {
		if(!rangeOverlaps(range1, range2)) {
			throw Exception("Attempt to add non-overlapping ranges")
		}
		Range(range1.start.min(range2.start), range1.end.max(range2.end))
	}
 	
	// returns a range with the start and end x coords of the region inside the manhattan radius of the 
	// sensor and beacon, or none if there is no such range at the y coord.
	def readingSlice(reading:Reading, targetVal:Int): Option[Range] = {
		val dist = manhattanDist(reading.sensor, reading.beacon)
		if((reading.sensor.y - targetVal).abs > dist) {
			None
		}
		else {
			val minX = reading.sensor.x - (dist - (reading.sensor.y - targetVal).abs)
			val maxX = reading.sensor.x + (dist - (reading.sensor.y - targetVal).abs)
			Some(Range(minX, maxX))
		}
	}
	
	def manhattanDist(start:Coord, end:Coord): Int =  (start.x - end.x).abs + (start.y - end.y).abs
	
	def parseReading(input: String): Option[Reading] = {
		val reading = "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)".r
		input match {
			case reading(sensorX, sensorY, beaconX, beaconY)	=> 	Some(Reading(Coord(sensorX.toInt, sensorY.toInt), Coord(beaconX.toInt, beaconY.toInt)))
			case _	=>	None
		}
	}
}

