//
// r2d3: https://rstudio.github.io/r2d3
//

var dataNodes, dataEdges;

//var edgesPromise = d3.csv('https://raw.githubusercontent.com/mrppdex/mrppdex.github.io/master/dataviz/data/lazegaEdges%401.csv', d3.autoType).then(data => { dataEdges = data });
//var nodesPromise = d3.csv('https://raw.githubusercontent.com/mrppdex/mrppdex.github.io/master/dataviz/data/lazegaNodes%402.csv', d3.autoType).then(data => { dataNodes = data });

dataNodes = data.vattr;
dataEdges = data.edges;


drag = simulation => {
  
  function dragstarted(event) {
    if (!event.active) simulation.alphaTarget(0.3).restart();
    event.subject.fx = event.subject.x;
    event.subject.fy = event.subject.y;
  }
  
  function dragged(event) {
    event.subject.fx = event.x;
    event.subject.fy = event.y;
  }
  
  function dragended(event) {
    if (!event.active) simulation.alphaTarget(0);
    event.subject.fx = null;
    event.subject.fy = null;
  }
  
  return d3.drag()
      .on("start", dragstarted)
      .on("drag", dragged)
      .on("end", dragended);
}

function chartFun() {
  //var width=600, height=600;
  //var svg = d3.select('body').append('svg').attr('width', width).attr('height', height);
  
  let node_radius = 4;
  let line_path = d3.line();

  const simulation = d3.forceSimulation()
      .force("charge", d3.forceManyBody()
             .strength(-300))
      .force("link", d3.forceLink().id(d => d.id))
      .force("x", d3.forceX())
      .force("y", d3.forceY())
      .force("center", d3.forceCenter(width / 2, height / 2))
      .on("tick", ticked);
      
  svg.select('defs').remove();
  svg.selectAll('g').remove();
  
  svg.append("defs")
    .append("marker")
      .attr("id", "arrhead")
      .attr("viewBox", "0 -5 10 10")
      .attr("refX", 15)
      .attr("refY", -1.5)
      .attr("markerWidth", 6)
      .attr("markerHeight", 6)
      .attr("orient", "auto")
    .append("path")
      .attr("d", "M0,-5L10,0L0,5");

  let g = svg.append("g");

  let link_g = g.append("g")
      .attr("stroke", "#000")
      .attr("stroke-width", 1)
      .attr("stroke-opacity", .3);
  //let link = link_g.selectAll("line");
  
  var link = link_g.append("g").selectAll("path")
    .data(simulation.force("link").links())
  .enter().append("path")
    .attr("marker-end", "url(#arrhead)")

  let node_g = g.append("g")
      .attr("stroke", "#fff")
      .attr("stroke-width", 1.5)
      .attr("cursor", "crosshair");
  let node = node_g.selectAll("circle");

  function ticked() {

    node.attr("cx", d => d.x)
        .attr("cy", d => d.y)

    //link.attr("x1", d => d.source.x)
    //    .attr("y1", d => d.source.y)
    //    .attr("x2", d => d.target.x)
    //    .attr("y2", d => d.target.y);

    link.attr("d", (d) => "M" + d.source.x + "," + d.source.y + ", " + d.target.x + "," + d.target.y)
      .attr("marker-end", (d) => options.directed?"url(#arrhead)":"");
  }
  
  function zoomed({transform}) {
    g.attr("transform", transform);
    //console.log(transform);
    node_radius = 4/transform.k;
    g.selectAll("circle").attr("r", node_radius);
    node_g.attr("stroke-width", 1.5/transform.k);
    link_g.attr("stroke-width", 1/transform.k);
  }
  
  var zoom = d3.zoom()
    .scaleExtent([.1, 10])    //.translateExtent([[0, 0], [width, height]])
    .on("zoom", zoomed);
   
  svg.call(zoom);

  return Object.assign( svg.node(), {
    update({nodes, links, covariate}) {
      
      var color = d3.scaleOrdinal(d3.schemeSet2);
      
      legend(nodes, covariate, color); 
      
      const old = new Map(node.data().map(d => [d.id, d]));
      nodes = nodes.map(d => Object.assign(old.get(d.id) || {}, d));
      links = links.map(d => Object.assign({}, d));

      node = node
        .data(nodes, d => d.id)
        .join(enter => enter.append("circle")
          .attr("r", node_radius)
          .attr("fill", d => color(d[covariate])))
        .call(node => {
          node.append("title").text(d => covariate + ": " + d[covariate]);
          console.log(options.category);
        })
        .call(drag(simulation))
        .on("mouseover", function() {
          Shiny.setInputValue(
            "d3_drag",
            JSON.stringify(d3.select(this).data()), {priority: "event"}
          )
        });

      link = link
        .data(links, d => [d.source, d.target])
        .join("path");

      simulation.nodes(nodes);
      simulation.force("link").links(links);
      simulation.force("charge", d3.forceManyBody().strength(-295*Math.exp(-1e-2*nodes.length)-5))
      simulation.alpha(1).restart();
  }});
}

function legend(nodes, category, colorScale) {
  let uniqueVals = nodes.map( d => d[category]).filter((v, i, a) => a.indexOf(v) === i).sort();
  
  uniqueVals.unshift(category);
  
  let legendSvg = svg.append("g");
  
  legendSvg.selectAll("text")
      .data(uniqueVals)
      .enter()
      .append("text")
      .attr("x", width-50)
      .attr("y", (d, i) => { return(50 + 20*i) })
      .attr("text-anchor", "end")
      .attr("font-family", "monospace")
      .text((d) => d);
      
  uniqueVals.shift();
  
  legendSvg.selectAll("circle")
    .data(uniqueVals)
    .enter()
    .append("circle")
    .attr("r", 5)
    .attr("cx", width-40)
    .attr("cy", (d, i) => { return(45 + 20*(i+1)) })
    .attr("fill", d => colorScale(d))
    .attr("stroke", "#fff")
    .attr("stroke-width", 1.5)
}

function update() {
  const nodes = dataNodes;
  const links = dataEdges;
  const covariate = options.category; //"Age","Gender","Office","Practice","School","vertex.names","Years" 

  chartObj = chartFun();
  chartObj.update({nodes, links, covariate});
}

function main() {
  Promise.all([nodesPromise, edgesPromise]).then( () => update());
}

update();