package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day12Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day12Definitions {
	case class Coord(x:Int, y:Int)
	case class State(grid:Seq[Seq[Char]], width:Int, height:Int, startCoord:Coord, endCoord:Coord)
	case class DNode(coord:Coord, height:Char, var dist:Int)
}

object Day12 extends PuzzleDay[Seq[String], Seq[String], Int, Int] {
	override def parse1(inputPath: String): Seq[String] = readLines(inputPath)
	override def parse2(inputPath: String): Seq[String] = parse1(inputPath)
	
	override def part1(lines: Seq[String]): Int = {
		dijkstra(parseLines(lines), isPart2 = false)
	}
	
	override def part2(lines: Seq[String]): Int = {
		dijkstra(parseLines(lines), isPart2 = true)
	}
	
	def parseLines(lines:Seq[String]): State = {
		var grid = lines.map(_.replace("S", "a")).map(_.replace("E", "z")).foldLeft(Seq.newBuilder[Seq[Char]])(_ += _.toCharArray.toSeq).result()
		var startX = 0
		var startY = 0
		var endX = 0
		var endY = 0
		val height = grid.size
		val width = grid(0).size
		for(i <- 0 until lines.size) {
			val s = lines(i).indexOf("S")
			val e = lines(i).indexOf("E")
			if(s != -1) {
				startY = i
				startX = s
			}
			if(e != -1) {
				endY = i
				endX = e
			}
		}
		State(grid, width, height, Coord(startX,startY), Coord(endX,endY))
	}
	
	def dijkstra(state:State, isPart2:Boolean = false): Int = {
		var frontier:scala.collection.mutable.Map[Coord, DNode] = scala.collection.mutable.Map()
		var frontierNext:scala.collection.mutable.Map[Coord, DNode] = scala.collection.mutable.Map()
		var explored:scala.collection.mutable.Map[Coord, DNode] = scala.collection.mutable.Map()
		if(isPart2) {
			// add the end node
			frontierNext(state.endCoord) = DNode(state.endCoord, state.grid(state.endCoord.y)(state.endCoord.x), 0)
		}
		else {
			// add the start node
			frontierNext(state.startCoord) = DNode(state.startCoord, state.grid(state.startCoord.y)(state.startCoord.x), 0)
		}
		while(frontierNext.size > 0) {
			frontier.clear
			for(coord <- frontierNext.keys) {
				var node = frontierNext(coord)
				frontier(coord) = node
			}
			frontierNext.clear
			for(coord <- frontier.keys) {
				var node = frontier(coord)
				if(explored.contains(coord)) {
					if(explored(coord).dist > frontier(coord).dist) {
						explored(coord).dist = frontier(coord).dist
					}
				}
				else {
					explored(coord) = DNode(coord, state.grid(coord.y)(coord.x), frontier(coord).dist)
				}
				val nextDist = node.dist + 1
				for(i <- 0 to 3) {
					var xd = 0
					var yd = 0
					if(i == 0) {
						yd = 1
					}
					if(i == 1) {
						xd = 1
					}
					if(i == 2) {
						yd = -1
					}
					if(i == 3) {
						xd = -1
					}
					val nextCoord = Coord(node.coord.x + xd, node.coord.y + yd)
					var delta = 0
					var addNext = true
					if(nextCoord.x < 0 || nextCoord.x >= state.width || nextCoord.y < 0 || nextCoord.y >= state.height) {
						addNext = false
					}
					else {
						delta = state.grid(nextCoord.y)(nextCoord.x) - state.grid(node.coord.y)(node.coord.x)
					}
					if(addNext && ((!isPart2 && delta > 1) || (isPart2 && delta < -1) )) {
						addNext = false
					}
					if(addNext && (explored.contains(nextCoord) && explored(nextCoord).dist <= nextDist)) {
						addNext = false
					}
					if(addNext && (frontierNext.contains(nextCoord) && frontierNext(nextCoord).dist <= nextDist)) {
						addNext = false
					}
					if(addNext) {
						frontierNext(nextCoord) = DNode(nextCoord, state.grid(nextCoord.y)(nextCoord.x), nextDist)
					}
					
				}
			}
		}
		
		if(isPart2) {
			explored.toSeq.map(_._2).filter(n => n.height == 'a').sortWith(_.dist < _.dist)(0).dist
		}
		else {
			if(!explored.contains(state.endCoord)) {
				throw Exception("Couldn't reach the end node")
			}
			else {
				explored(state.endCoord).dist
			}
		}
	}
	
}
