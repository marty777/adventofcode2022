package com.marty777.adventofcode2022

import com.marty777.adventofcode2022.Day19Definitions._
import com.marty777.adventofcode2022.PuzzleDay
import com.marty777.util.FileLines.readLines

object Day19Definitions {
	enum RobotType:
		case Ore,Clay,Obsidian,Geode
	enum MaterialType:
		case Ore,Clay,Obsidian,Geode
	case class Blueprint(id:Int, oreRobotOre:Int, clayRobotOre:Int, obsidianRobotOre:Int, obsidianRobotClay:Int, geodeRobotOre:Int, geodeRobotObsidian:Int)
	case class State(steps:Int, oreRobots:Int, clayRobots:Int, obsidianRobots:Int, geodeRobots:Int, ore:Int, clay:Int, obsidian:Int, geode:Int)
}

object Day19 extends PuzzleDay[Seq[Blueprint], Seq[Blueprint], Int, Int] {
	override def parse1(inputPath: String): Seq[Blueprint] = readLines(inputPath).flatMap(parseBlueprint)
	override def parse2(inputPath: String): Seq[Blueprint] = parse1(inputPath)

	override def part1(blueprints: Seq[Blueprint]): Int =  {
		val limit = 25 // run to the start of minute 25 (i.e. the end of minute 24)
		blueprints.map(b => b.id * search(b, limit)).sum
	}
	
	override def part2(blueprints: Seq[Blueprint]): Int =  {
		val limit = 33 // run to the start of minute 33 (i.e. the end of minute 32)
		val topBlueprints = blueprints.take(3)
		topBlueprints.foldLeft(1)((acc, b) => acc * search(b, limit))
	}
	
	// Not exactly a bfs, but close enough. We step through states by robot 
	// build order, so some states will be at different minutes on each step 
	// processing the queue. Prune out states that aren't meeting a best-seen 
	// geode threshold at each minute. It's fast enough, but it may not reach 
	// the best possible value if the pruning threshold is too low.
	def search(blueprint:Blueprint, cutoff:Int):Int = {
		var queue:scala.collection.mutable.Queue[State] = scala.collection.mutable.Queue()
		var nextQueue:scala.collection.mutable.Queue[State] = scala.collection.mutable.Queue()
		var previouslySeen:scala.collection.mutable.Map[Seq[Int], Boolean] = scala.collection.mutable.Map()
		var bestAtStep:scala.collection.mutable.Seq[Int] = scala.collection.mutable.Seq()
		for(i <- 0 to cutoff) {
			bestAtStep = bestAtStep :+ 0
		}
		var best = 0
		// we don't need to produce more material per minute than can be used in bot construction, so determine the maximums
		var maxOreRobots = blueprint.oreRobotOre.max(blueprint.clayRobotOre).max(blueprint.obsidianRobotOre).max(blueprint.geodeRobotOre)
		var maxClayRobots = blueprint.obsidianRobotClay
		var maxObsidianRobots = blueprint.geodeRobotObsidian 
		
		// there's a further refinement involving not building any more of a robot if we have a stockpile
		// that we couldn't exhaust by building any other robots over the remaining time. Maybe later.
		
		val pruneThreshold = 2 // setting this below two seems to skip over some better solutions. going higher won't prune as many states on each round.
		
		val startState = State(1, 1, 0, 0, 0, 0, 0, 0, 0)
		nextQueue.enqueue(startState)
		while(nextQueue.size > 0) {
			queue = nextQueue.clone
			nextQueue.clear
			while(queue.size > 0) {
				val state = queue.dequeue
				if(bestAtStep(state.steps) < state.geode) {
					bestAtStep(state.steps) = state.geode
				}
				// don't advance any state that's worse than the best known geode performance at this step less the prune threshold.
				if(state.geode >= bestAtStep(state.steps) - pruneThreshold && state.steps < cutoff) {
					
					// if a geode bot could be built eventually if it was the next bot in the build order.
					if(state.obsidianRobots > 0 && state.oreRobots > 0) {
						val nextState = buildNextRobot(blueprint, state, RobotType.Geode, cutoff)
						val nextKey = stateKey(nextState)
						if(!previouslySeen.contains(nextKey) && bestAtStep(nextState.steps) <= nextState.geode + pruneThreshold) {
							nextQueue.enqueue(nextState)
							previouslySeen(nextKey) = true
						}
					}
					// if an obsidian bot could be built eventually if it was the next bot in the build order, and we aren't at the maximum
					if(state.clayRobots > 0 && state.oreRobots > 0 && state.obsidianRobots < maxObsidianRobots) {
						val nextState = buildNextRobot(blueprint, state, RobotType.Obsidian, cutoff)
						val nextKey = stateKey(nextState)
						if(!previouslySeen.contains(nextKey) && bestAtStep(nextState.steps) <= nextState.geode + pruneThreshold) {
							nextQueue.enqueue(nextState)
							previouslySeen(nextKey) = true
						}
					}
					// if a clay bot could be built eventually if it was the next bot in the build order, and we aren't at the maximum
					if(state.oreRobots > 0 && state.clayRobots < maxClayRobots) {
						val nextState = buildNextRobot(blueprint, state, RobotType.Clay, cutoff)
						val nextKey = stateKey(nextState)
						if(!previouslySeen.contains(nextKey) && bestAtStep(nextState.steps) <= nextState.geode + pruneThreshold) {
							nextQueue.enqueue(nextState)
							previouslySeen(nextKey) = true
						}
					}
					// if an ore bot could be built eventually if it was the next bot in the build order, and we aren't at the maximum
					if(state.oreRobots > 0 && state.oreRobots < maxOreRobots) {
						val nextState = buildNextRobot(blueprint, state, RobotType.Ore, cutoff)
						val nextKey = stateKey(nextState)
						if(!previouslySeen.contains(nextKey) && bestAtStep(nextState.steps) <= nextState.geode + pruneThreshold) {
							nextQueue.enqueue(nextState)
							previouslySeen(nextKey) = true
						}
					}
					
				}
			}
		}
		bestAtStep(cutoff)
	}
	
	// advance the state to the specified minute without building any bots
	def advanceState(state:State, stopSteps:Int):State = {
		var steps = stopSteps - state.steps
		var ore = state.ore + (steps * state.oreRobots)
		var clay = state.clay + (steps * state.clayRobots)
		var obsidian = state.obsidian + (steps * state.obsidianRobots)
		var geode = state.geode + (steps * state.geodeRobots)
		State(state.steps + steps, state.oreRobots, state.clayRobots, state.obsidianRobots, state.geodeRobots, ore, clay, obsidian, geode)
	}
	
	// determine how many steps are needed to accumulate the specified material amount
	def stepsToMaterial(state:State, materialType:MaterialType, requiredAmount:Int):Int = {
		var materialAmount = 0
		var materialRobots = 0
		materialType match {
			case MaterialType.Ore => {
				materialAmount = state.ore
				materialRobots = state.oreRobots
				
			}
			case MaterialType.Clay => {
				materialAmount = state.clay
				materialRobots = state.clayRobots
			}
			case MaterialType.Obsidian => {
				materialAmount = state.obsidian
				materialRobots = state.obsidianRobots
			}
			// shouldn't need this, but...
			case MaterialType.Geode => {
				materialAmount = state.geode
				materialRobots = state.geodeRobots
			}
		}
		if(materialAmount >= requiredAmount) {
			0
		}
		else {
			var steps = (requiredAmount - materialAmount) / materialRobots
			if((steps * materialRobots) + materialAmount < requiredAmount) {
				steps += 1
			}
			steps
		}
	}
	
	// If the resources are available, build the robot. 
	// Otherwise, advance the state until the resources are ready then build the robot.
	def buildNextRobot(blueprint:Blueprint, state:State, robot:RobotType, stopSteps:Int):State = {
		var stepsToRobot = 0
		var robotList = Seq(0,0,0,0)
		var materialsList = Seq(0,0,0)
		robot match {
			case RobotType.Ore => {
				if(state.oreRobots == 0) {
					throw Exception("Trying to build an ore robot with no ore robots producing")
				}
				materialsList = Seq(blueprint.oreRobotOre,0,0)
				robotList = Seq(1,0,0,0)
				val oreSteps = stepsToMaterial(state, MaterialType.Ore,blueprint.oreRobotOre)
				stepsToRobot = oreSteps
			}
			case RobotType.Clay => {
				if(state.oreRobots == 0) {
					throw Exception("Trying to build a clay robot with no ore robots producing")
				}
				materialsList = Seq(blueprint.clayRobotOre,0,0)
				robotList = Seq(0,1,0,0)
				val oreSteps = stepsToMaterial(state, MaterialType.Ore,blueprint.clayRobotOre)
				stepsToRobot = oreSteps
			}
			case RobotType.Obsidian => {
				if(state.oreRobots == 0 || state.clayRobots == 0) {
					throw Exception("Trying to build an obsidian robot without enough ore and clay robots producing")
				}
				materialsList = Seq(blueprint.obsidianRobotOre, blueprint.obsidianRobotClay,0)
				robotList = Seq(0,0,1,0)
				val oreSteps = stepsToMaterial(state, MaterialType.Ore,blueprint.obsidianRobotOre)
				val claySteps = stepsToMaterial(state, MaterialType.Clay,blueprint.obsidianRobotClay)
				stepsToRobot = oreSteps.max(claySteps)
			}
			case RobotType.Geode => {
				if(state.oreRobots == 0 || state.obsidianRobots == 0) {
					throw Exception("Trying to build a geode robot without enough ore and obsidian robots producing")
				}
				materialsList = Seq(blueprint.geodeRobotOre, 0, blueprint.geodeRobotObsidian)
				robotList = Seq(0,0,0,1)
				val oreSteps = stepsToMaterial(state, MaterialType.Ore,blueprint.geodeRobotOre)
				val obsidianSteps = stepsToMaterial(state, MaterialType.Obsidian,blueprint.geodeRobotObsidian)
				stepsToRobot = oreSteps.max(obsidianSteps)
			}
		}
		// The robot can't be built before we run out of time. Horde resources
		// until time runs out instead.
		if(state.steps + stepsToRobot >= stopSteps) {
			advanceState(state, stopSteps)
		}
		else {
			val nextState = advanceState(state, state.steps + stepsToRobot + 1)
			val finalState = State(nextState.steps, 
								nextState.oreRobots + robotList(0),
								nextState.clayRobots + robotList(1),
								nextState.obsidianRobots + robotList(2),
								nextState.geodeRobots + robotList(3),
								nextState.ore - materialsList(0),
								nextState.clay - materialsList(1),
								nextState.obsidian - materialsList(2),
								nextState.geode)
			finalState
		}
	}
	
	def stateKey(state:State):Seq[Int] = Seq(state.steps, state.oreRobots, state.clayRobots, state.obsidianRobots, state.geodeRobots, state.ore, state.clay, state.obsidian, state.geode)
	
	def parseBlueprint(line: String): Option[Blueprint] = {
		val blueprint = "Blueprint (.*): Each ore robot costs (.*) ore. Each clay robot costs (.*) ore. Each obsidian robot costs (.*) ore and (.*) clay. Each geode robot costs (.*) ore and (.*) obsidian.".r
		line match {
			case blueprint(id, oreRobotOre, clayRobotOre, obsidianRobotOre, obsidianRobotClay, geodeRobotOre, geodeRobotObsidian) 
				=> Some(Blueprint(id.toInt, oreRobotOre.toInt, clayRobotOre.toInt, obsidianRobotOre.toInt, obsidianRobotClay.toInt, geodeRobotOre.toInt, geodeRobotObsidian.toInt))
			case _ 
				=> None
		}
	}
	
}

