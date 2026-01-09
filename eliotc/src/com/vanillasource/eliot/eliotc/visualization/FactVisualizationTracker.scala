package com.vanillasource.eliot.eliotc.visualization

import cats.effect.{IO, Ref}
import FactVisualizationTracker.GraphData
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerFactKey

import java.nio.file.{Files, Path}
import scala.collection.mutable

/** Tracks fact generation and consumption between processors to build a visualization graph. Nodes represent
  * processors, edges represent fact types flowing between them.
  */
final class FactVisualizationTracker(
    // Mapping: FactTypeName -> (Processors that were requested, Processors that just produced without being asked)
    factProducers: Ref[IO, Map[String, (Set[String], Set[String])]],
    // Mapping: Processor -> FactTypeName
    factRequests: Ref[IO, List[(String, String)]],
    // Keys that were requested up until now (for determining whether fact was asked for)
    factRequestedKeys: Ref[IO, Set[CompilerFactKey[?]]]
) extends Logging {

  /** Record that a processor produced a fact */
  def recordFactProduction(processorName: String, factKey: CompilerFactKey[?]): IO[Unit] = {
    val factTypeName = getFactTypeName(factKey)
    for {
      requestedKeys <- factRequestedKeys.get
      _             <- if (requestedKeys.contains(factKey)) {
                         // This key was previously requested, so we assume this processor is fulfilling a request
                         factProducers.update(
                           _.updatedWith(factTypeName) { processorsMaybe =>
                             val processors = processorsMaybe.getOrElse((Set.empty, Set.empty))

                             Some((processors._1 + processorName, processors._2))
                           }
                         )
                       } else {
                         // This key was not requested, we assume this processor initiates a new workflow of sorts
                         factProducers.update(
                           _.updatedWith(factTypeName) { processorsMaybe =>
                             val processors = processorsMaybe.getOrElse((Set.empty, Set.empty))

                             Some((processors._1, processors._2 + processorName))
                           }
                         )
                       }
    } yield ()
  }

  /** Record that a processor requested a fact */
  def recordFactRequest(processorName: String, factKey: CompilerFactKey[?]): IO[Unit] = {
    val factTypeName = getFactTypeName(factKey)
    factRequestedKeys.update(_ + factKey) >> factRequests.update(_ :+ (processorName, factTypeName))
  }

  /** Generate HTML visualization using Cytoscape.js */
  def generateVisualization(outputPath: Path): IO[Unit] =
    for {
      producers <- factProducers.get
      requests  <- factRequests.get
      graphData <- buildGraphData(producers, requests)
      html       = generateHTML(graphData)
      _         <- IO.blocking(Files.writeString(outputPath, html))
      _         <- info[IO](s"Fact visualization saved to: $outputPath")
    } yield ()

  private def buildGraphData(
      producers: Map[String, (Set[String], Set[String])],
      requests: List[(String, String)]
  ): IO[GraphData] = IO {
    val edges = mutable.Map[(String, String, String), Int]()
    val nodes = mutable.Set[String]()

    // Build edges from requests: fact flows from producer to consumer
    requests.foreach { case (consumer, factType) =>
      producers.get(factType).foreach { producerSets =>
        producerSets._1.foreach { producer =>
          if (producer != consumer) {
            val key = (producer, consumer, factType)
            edges(key) = edges.getOrElse(key, 0) + 1
            nodes += producer
            nodes += consumer
          }
        }
      }
    }

    // Add nodes that only produce facts (no consumers yet)
    producers.values.map(_._1).flatten.foreach(nodes += _)

    GraphData(nodes.toSet, edges.toMap)
  }

  private def getFactTypeName(factKey: CompilerFactKey[?]): String = {
    val fullName = factKey.getClass.getName
    // Remove package prefix and $Key suffix for cleaner display
    fullName
      .replaceAll(".*\\.", "")
      .replaceAll("\\$Key$", "")
      .replaceAll("\\$", ".")
  }

  private def generateHTML(graphData: GraphData): String = {
    // Calculate statistics for dynamic scaling
    val counts   = graphData.edges.values.toSeq
    val minCount = if (counts.isEmpty) 1 else counts.min
    val maxCount = if (counts.isEmpty) 1 else counts.max
    val logMin   = math.log(minCount.toDouble)
    val logMax   = math.log(maxCount.toDouble)

    // Calculate ranks first (shortest path from sources), then derive back-edges from ranks
    val ranks     = calculateRanks(graphData)
    val backEdges = findBackEdges(graphData, ranks)

    val nodeElements = graphData.nodes.toSeq.sorted.zipWithIndex
      .map { case (name, idx) =>
        val rank = ranks.getOrElse(name, 0)
        s"""{ data: { id: '$name', label: '${simplifyProcessorName(name)}', rank: $rank } }"""
      }
      .mkString(",\n            ")

    val edgeElements = graphData.edges.toSeq
      .sortBy { case ((producer, _, _), _) => producer }
      .zipWithIndex
      .map { case (((producer, consumer, factType), count), idx) =>
        val edgeId        = s"edge_$idx"
        // Exponential scaling: map count logarithmically to edge width (1-10 range)
        val logCount      = math.log(count.toDouble)
        val normalizedLog = if (logMax > logMin) (logCount - logMin) / (logMax - logMin) else 0.5
        val edgeWidth     = 1 + (normalizedLog * 9) // Scale to 1-10 range

        val isBackEdge = backEdges.contains((producer, consumer, factType))

        s"""{ data: { id: '$edgeId', source: '$producer', target: '$consumer', label: '${simplifyFactName(
            factType
          )}', count: $count, width: ${edgeWidth.round}, backEdge: $isBackEdge } }"""
      }
      .mkString(",\n            ")

    s"""<!DOCTYPE html>
<html>
<head>
    <title>ELIOT Compiler - Fact Generation Visualization</title>
    <meta charset="utf-8">
    <style>
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            margin: 0;
            padding: 0;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            display: flex;
            flex-direction: column;
            height: 100vh;
        }
        #header {
            background: rgba(255, 255, 255, 0.95);
            padding: 20px 30px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
            z-index: 1000;
        }
        #header h1 {
            margin: 0 0 10px 0;
            color: #333;
            font-size: 28px;
        }
        #header p {
            margin: 0;
            color: #666;
            font-size: 14px;
        }
        #cy {
            flex: 1;
            background: white;
            margin: 20px;
            border-radius: 10px;
            box-shadow: 0 4px 20px rgba(0,0,0,0.1);
        }
        #info-panel {
            position: absolute;
            top: 100px;
            right: 40px;
            background: rgba(255, 255, 255, 0.95);
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 4px 20px rgba(0,0,0,0.15);
            max-width: 300px;
            display: none;
        }
        #info-panel h3 {
            margin: 0 0 10px 0;
            color: #333;
            font-size: 18px;
        }
        #info-panel p {
            margin: 5px 0;
            color: #666;
            font-size: 13px;
        }
        .legend {
            position: absolute;
            bottom: 40px;
            left: 40px;
            background: rgba(255, 255, 255, 0.95);
            padding: 15px;
            border-radius: 8px;
            box-shadow: 0 4px 20px rgba(0,0,0,0.15);
        }
        .legend h4 {
            margin: 0 0 10px 0;
            color: #333;
            font-size: 14px;
        }
        .legend-item {
            display: flex;
            align-items: center;
            margin: 5px 0;
            font-size: 12px;
            color: #666;
        }
        .legend-color {
            width: 20px;
            height: 20px;
            border-radius: 50%;
            margin-right: 10px;
        }
    </style>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.23.0/cytoscape.min.js"></script>
    <script src="https://unpkg.com/dagre@0.8.5/dist/dagre.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/cytoscape-dagre@2.5.0/cytoscape-dagre.min.js"></script>
</head>
<body>
    <div id="header">
        <h1>🔍 ELIOT Compiler - Fact Generation Flow</h1>
        <p>Visualizing how facts flow between compiler processors. Click on nodes or edges to see details.</p>
    </div>
    <div id="cy"></div>
    <div id="info-panel"></div>
    <div class="legend">
        <h4>Legend</h4>
        <div class="legend-item">
            <div class="legend-color" style="background: #4285F4;"></div>
            <span>Processor Node</span>
        </div>
        <div class="legend-item">
            <div class="legend-color" style="background: #34A853; border-radius: 0; width: 30px; height: 3px;"></div>
            <span>Forward Edge (normal flow)</span>
        </div>
        <div class="legend-item">
            <div class="legend-color" style="background: #FF6D00; border-radius: 0; width: 30px; height: 3px;"></div>
            <span>Back Edge (cycle/dynamic)</span>
        </div>
    </div>
    <script>
        var cy = cytoscape({
            container: document.getElementById('cy'),

            elements: [
            $nodeElements,
            $edgeElements
            ],

            style: [
                {
                    selector: 'node',
                    style: {
                        'background-color': '#4285F4',
                        'label': 'data(label)',
                        'color': '#fff',
                        'text-valign': 'center',
                        'text-halign': 'center',
                        'font-size': '11px',
                        'font-weight': 'bold',
                        'width': '120px',
                        'height': '80px',
                        'border-width': 3,
                        'border-color': '#1967D2',
                        'text-outline-color': '#4285F4',
                        'text-outline-width': 2,
                        'text-wrap': 'wrap',
                        'text-max-width': '110px'
                    }
                },
                {
                    selector: 'node:selected',
                    style: {
                        'background-color': '#34A853',
                        'border-color': '#188038',
                        'border-width': 4
                    }
                },
                {
                    selector: 'edge',
                    style: {
                        'width': 'data(width)',
                        'line-color': '#34A853',
                        'target-arrow-color': '#34A853',
                        'target-arrow-shape': 'triangle',
                        'curve-style': 'bezier',
                        'label': 'data(label)',
                        'font-size': '9px',
                        'text-rotation': 'autorotate',
                        'text-margin-y': -10,
                        'color': '#5F6368',
                        'text-background-color': '#fff',
                        'text-background-opacity': 0.9,
                        'text-background-padding': '3px',
                        'arrow-scale': 1.2
                    }
                },
                {
                    selector: 'edge[?backEdge]',
                    style: {
                        'line-color': '#FF6D00',
                        'target-arrow-color': '#FF6D00',
                        'line-style': 'dashed',
                        'line-dash-pattern': [6, 3]
                    }
                },
                {
                    selector: 'edge:selected',
                    style: {
                        'line-color': '#FBBC04',
                        'target-arrow-color': '#FBBC04',
                        'width': 'data(width)'
                    }
                }
            ],

            layout: {
                name: 'dagre',
                rankDir: 'LR',
                nodeSep: 100,
                edgeSep: 50,
                rankSep: 150,
                animate: true,
                animationDuration: 500,
                fit: true,
                padding: 50,
                // Use explicit ranks from our calculation
                rank: function(node) {
                    return node.data('rank');
                },
                // Ignore back-edges for ranking by setting their minimum length to 0
                minLen: function(edge) {
                    return edge.data('backEdge') ? 0 : 1;
                }
            }
        });

        // Interaction handlers
        var infoPanel = document.getElementById('info-panel');

        cy.on('tap', 'node', function(evt){
            var node = evt.target;
            var incoming = node.incomers('edge');
            var outgoing = node.outgoers('edge');

            var incomingFacts = incoming.map(e => e.data('label') + ' (' + e.data('count') + ')').join(', ') || 'None';
            var outgoingFacts = outgoing.map(e => e.data('label') + ' (' + e.data('count') + ')').join(', ') || 'None';

            infoPanel.innerHTML = '<h3>' + node.data('label') + '</h3>' +
                '<p><strong>Consumes:</strong> ' + incomingFacts + '</p>' +
                '<p><strong>Produces:</strong> ' + outgoingFacts + '</p>' +
                '<p><strong>Rank:</strong> ' + node.data('rank') + '</p>' +
                '<p><strong>Total In:</strong> ' + incoming.length + ' edges</p>' +
                '<p><strong>Total Out:</strong> ' + outgoing.length + ' edges</p>';
            infoPanel.style.display = 'block';
        });

        cy.on('tap', 'edge', function(evt){
            var edge = evt.target;
            var backEdgeInfo = edge.data('backEdge') ?
                '<p><strong>Type:</strong> <span style="color: #FF6D00;">⚠️ Back Edge (creates cycle)</span></p>' :
                '<p><strong>Type:</strong> Forward Edge</p>';
            infoPanel.innerHTML = '<h3>' + edge.data('label') + '</h3>' +
                '<p><strong>From:</strong> ' + edge.source().data('label') + '</p>' +
                '<p><strong>To:</strong> ' + edge.target().data('label') + '</p>' +
                '<p><strong>Count:</strong> ' + edge.data('count') + ' fact(s)</p>' +
                backEdgeInfo;
            infoPanel.style.display = 'block';
        });

        cy.on('tap', function(evt){
            if (evt.target === cy) {
                infoPanel.style.display = 'none';
            }
        });

        // Fit and center the graph
        cy.fit();
        cy.center();
    </script>
</body>
</html>"""
  }

  /** Find back-edges based on node ranks. A back-edge is an edge that goes from a node to another node with the same or
    * lower rank (i.e., not moving forward in the DAG).
    */
  private def findBackEdges(graphData: GraphData, ranks: Map[String, Int]): Set[(String, String, String)] =
    graphData.edges.keys.filter { case (source, target, _) =>
      ranks.getOrElse(source, 0) >= ranks.getOrElse(target, 0)
    }.toSet

  /** Calculate the rank of each node as the longest directed path from any source node (nodes with no incoming edges).
    * Uses topological sort with dynamic programming. Nodes in cycles unreachable from sources get rank 0.
    */
  private def calculateRanks(graphData: GraphData): Map[String, Int] = {
    val outgoingEdges = graphData.edges.keys.groupBy(_._1).map((k, v) => k -> v.map(_._2))
    val incomingEdges = graphData.edges.keys.groupBy(_._2).map((k, v) => k -> v.map(_._1))

    // Find source nodes (no incoming edges)
    val sources = graphData.nodes.filter(node => !incomingEdges.contains(node))

    // Topological sort with longest path calculation (Kahn's algorithm)
    val ranks     = mutable.Map[String, Int]()
    val queue     = mutable.Queue[String]()
    val inDegree  = mutable.Map[String, Int]()
    val maxInRank = mutable.Map[String, Int]()

    // Initialize in-degrees and max incoming ranks
    graphData.nodes.foreach { node =>
      inDegree(node) = incomingEdges.getOrElse(node, Set.empty).size
      maxInRank(node) = -1
    }

    // Start from all sources at rank 0
    sources.foreach { source =>
      ranks(source) = 0
      queue.enqueue(source)
    }

    while (queue.nonEmpty) {
      val node     = queue.dequeue()
      val nodeRank = ranks(node)

      outgoingEdges.getOrElse(node, Set.empty).foreach { target =>
        maxInRank(target) = math.max(maxInRank(target), nodeRank)
        inDegree(target) -= 1

        if (inDegree(target) == 0) {
          ranks(target) = maxInRank(target) + 1
          queue.enqueue(target)
        }
      }
    }

    // Handle any unranked nodes (in cycles with no path from sources)
    graphData.nodes.foreach { node =>
      if (!ranks.contains(node)) {
        ranks(node) = 0
      }
    }

    ranks.toMap
  }

  private def simplifyProcessorName(fullName: String): String = {
    fullName
      .replaceAll(".*\\.", "")
      .replaceAll("Processor$", "")
      .replaceAll("([a-z])([A-Z])", "$1\\\\n$2") // Add escaped line breaks for readability
  }

  private def simplifyFactName(fullName: String): String = {
    fullName.replaceAll("([a-z])([A-Z])", "$1 $2")
  }
}

object FactVisualizationTracker {
  case class GraphData(nodes: Set[String], edges: Map[(String, String, String), Int])

  def create(): IO[FactVisualizationTracker] =
    for {
      producers         <- Ref.of[IO, Map[String, (Set[String], Set[String])]](Map.empty)
      requests          <- Ref.of[IO, List[(String, String)]](List.empty)
      requestedFactKeys <- Ref.of[IO, Set[CompilerFactKey[?]]](Set.empty)
    } yield new FactVisualizationTracker(producers, requests, requestedFactKeys)
}
