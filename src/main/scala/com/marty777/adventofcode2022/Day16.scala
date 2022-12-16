package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day16Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day16Definitions {
	case class Valve(id:String, flow:Int, tunnels:Seq[String])
	case class State(valves: Map[String, Valve])
	case class StateNode(pressure:Int, steps: Int, open:Seq[String], position:String)
	case class ElephantNode(pressure:Int, steps: Int, open:Seq[String], elephantPosition:String, elephantDestination:String, myPosition:String, myDestination:String)
}

object Day16 extends PuzzleDay[Seq[Valve], Seq[Valve], Long, Long] {
	override def parse1(inputPath: String): Seq[Valve] = readLines(inputPath).flatMap(parseValve)
	override def parse2(inputPath: String): Seq[Valve] = parse1(inputPath)
	
	
	// notes for later: Djikstra to establish all accessible nodes. Filter for important -> flow > 0. cache 
	// path lengths between all accessible important nodes. Djikstra across scenarios for best
	// route to 30 steps.
	override def part1(valves: Seq[Valve]): Long = {
		var valveMap:Map[String,Valve] = Map()
		for(valve <- valves) {
			valveMap = valveMap.updated(valve.id, valve)
		}
		// not all valves are accessible, and only some have flow > 0.
		val accessible = dijkstra_Accessible(valveMap)
		var importantValves:Seq[String] = Seq()
		for(id <- accessible) {
			if(valveMap(id).flow > 0) {
				importantValves = importantValves :+ id
			}
		}
		// build a cache of distances between all accessible valves
		var pathMap:Map[(String,String),Int] = Map()
		for(i <- 0 until accessible.size) {
			for(j <- 0 until accessible.size) {
				if(i != j) {
					pathMap = pathMap.updated( (accessible(i), accessible(j)), dijkstra_BestPath(valveMap, accessible(i), accessible(j)) )
				}
			}
		}
		dijkstra_ReleasePressure(valveMap, importantValves, pathMap, isPart1 = true)
	}
	
	override def part2(valves: Seq[Valve]): Long = {
		
		var valveMap:Map[String,Valve] = Map()
		for(valve <- valves) {
			valveMap = valveMap.updated(valve.id, valve)
		}
		val accessible = dijkstra_Accessible(valveMap)
		var importantValves:Seq[String] = Seq()
		for(id <- accessible) {
			if(valveMap(id).flow > 0) {
				importantValves = importantValves :+ id
			}
		}
		if(importantValves.size > 6) {
			println("This may take a few minutes...")
		}
		
		var pathMap:Map[(String,String),Int] = Map()
		for(i <- 0 until accessible.size) {
			for(j <- 0 until accessible.size) {
				if(i != j) {
					pathMap = pathMap.updated( (accessible(i), accessible(j)), dijkstra_BestPath(valveMap, accessible(i), accessible(j)) )
				}
			}
		}
		
		// The movement of the elephant and the player are independant.
		// Split the list of important valves between the elephant and the player
		// in all possible partitionings. Add the best pressure released by the 
		// player and the elephant by opening their separate sets of goal valves. 
		// Find the best result over all partitions of the important valves
		// This isn't quick, but it works.
		val bitsMax = 1 << importantValves.size
		var bestPressure = -1
		for(bits <- 0 until bitsMax) {
			var myValves:Seq[String] = Seq()
			var elephantValves:Seq[String] = Seq()
			for(i <- 0 until importantValves.size) {
				val choice = (bits >> (i)) & 0x01
				if(choice == 0) {
					myValves = myValves :+ importantValves(i)
				}
				else {
					elephantValves = elephantValves :+ importantValves(i)
				}
			}
			var myPressure = dijkstra_ReleasePressure(valveMap, myValves, pathMap, isPart1 = false)
			var elephantPressure = dijkstra_ReleasePressure(valveMap, elephantValves, pathMap, isPart1 = false)
			if(myPressure + elephantPressure > bestPressure) {
				bestPressure = myPressure + elephantPressure 
			}
			
		}
		bestPressure
	}
	
	def parseValve(input:String): Option[Valve] = {
		val valvePlural = "Valve (.*) has flow rate=(.*); tunnels lead to valves (.*)".r
		val valveSingular = "Valve (.*) has flow rate=(.*); tunnel leads to valve (.*)".r
		input match {
			case valvePlural(id, flowRate, list) => {
				val tunnels = list.split(", ").toSeq
				Some(Valve(id, flowRate.toInt, tunnels))
			}
			case valveSingular(id, flowRate, tunnel) => {
				val tunnels = Seq(tunnel)
				Some(Valve(id, flowRate.toInt, tunnels))
			}
			case _ => {
				None
			}
		}
	}
	
	// find all accessible valves in the system 
	def dijkstra_Accessible(valves:Map[String, Valve]): Seq[String] = {
		val frontier:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		val frontierNext:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		val explored:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		
		frontierNext("AA") = 0
		
		while(frontierNext.size > 0) {
			frontier.clear
			for(id <- frontierNext.keys) {
				frontier(id) = frontierNext(id)
			}
			frontierNext.clear
			for(id <- frontier.keys) {
				if(betterDist(explored, id, frontier(id))) {
					explored(id) = frontier(id)
				}
				val nextDist = frontier(id) + 1
				for(nextId <- valves(id).tunnels) {
					if(betterDist(explored,nextId,nextDist) && betterDist(frontierNext, nextId, nextDist) ) {
						frontierNext(nextId) = nextDist
					}
				}
			}
		}
		explored.toSeq.map(_._1)
	}
	
	// find the best distance between two accessible valves
	def dijkstra_BestPath(valves:Map[String, Valve], start:String, end:String): Int = {
		val frontier:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		val frontierNext:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		val explored:scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map()
		
		frontierNext(start) = 0
		
		while(frontierNext.size > 0) {
			frontier.clear
			for(id <- frontierNext.keys) {
				frontier(id) = frontierNext(id)
			}
			frontierNext.clear
			for(id <- frontier.keys) {
				if(betterDist(explored, id, frontier(id))) {
					explored(id) = frontier(id)
				}
				val nextDist = frontier(id) + 1
				for(nextId <- valves(id).tunnels) {
					if(betterDist(explored,nextId,nextDist) && betterDist(frontierNext, nextId, nextDist) ) {
						frontierNext(nextId) = nextDist
					}
				}
			}
		}
		if(!explored.contains(end)) {
			throw Exception(s"Could not find path between $start and $end")
		}
		explored(end)
	}
	
	// find the maximum amount of pressure released by a single actor opening all the important valves
	def dijkstra_ReleasePressure(valves:Map[String, Valve], importantValves:Seq[String], paths:Map[(String, String), Int], isPart1:Boolean): Int = {
		val frontier:scala.collection.mutable.Map[String, StateNode] = scala.collection.mutable.Map()
		val frontierNext:scala.collection.mutable.Map[String, StateNode] = scala.collection.mutable.Map()
		val explored:scala.collection.mutable.Map[String, StateNode] = scala.collection.mutable.Map()
		
		var stepsCutoff = 30
		if(!isPart1) {
			stepsCutoff = 26
		}
		
		frontierNext(stateKey(Seq(), "AA")) = StateNode(0,0,Seq(), "AA")
		
		while(frontierNext.size > 0) {
			frontier.clear
			for(key <- frontierNext.keys) {
				frontier(key) = frontierNext(key)
			}
			frontierNext.clear
			for(key <- frontier.keys) {
				if(betterPressure(explored, key, frontier(key).pressure)) {
					explored(key) = frontier(key)
				}
				for(important <- importantValves) {
					if(!frontier(key).open.contains(important)) {
						val nextSteps = frontier(key).steps + paths((frontier(key).position, important)) + 1
						val nextPressure = frontier(key).pressure + (valves(important).flow * (stepsCutoff - nextSteps))
						val nextOpen =  frontier(key).open :+ important
						val nextKey = stateKey(nextOpen, frontier(key).position)
						if(nextSteps <= stepsCutoff && betterPressure(explored, nextKey, nextPressure) && betterPressure(frontierNext, nextKey, nextPressure)) {
							frontierNext(nextKey) = StateNode(nextPressure, nextSteps, nextOpen, important)
						}
					}
				}
			}
		}
		explored.toSeq.map(_._2.pressure).sorted.reverse.head
	}
	
	// dijkstra helper functions
	def stateKey(visited: Seq[String], position:String): String = {
		position + ":" + visited.sorted.mkString("")
	}
	def betterDist(collection: scala.collection.mutable.Map[String, Int], id:String, newDist:Int): Boolean = {
		if(!collection.contains(id)) then true else collection(id) > newDist
	}
	def betterPressure(collection: scala.collection.mutable.Map[String, StateNode], key:String, newPressure:Int): Boolean = {
		if(!collection.contains(key)) then true else collection(key).pressure < newPressure
	}
	
	
}

