
		
// setting the dimensions and margins of the chart
var margin = {top: 20, right: 0, bottom: 50, left: 80},
    width2 = 500 - margin.left - margin.right,
    height2 = 350 - margin.top - margin.bottom;

// appending the svg object to the body of the page
var svg2 = d3.select("#_2_partisan").append("svg")
    .attr("width", width2 + margin.left + margin.right)
    .attr("height", height2 + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

// adding title
var Title = svg2.append("text")
		.attr("y", -5)
		.attr("font-size", "1.75em")
		.attr("text-anchor", "right")
		.text("Tweets about Politicians");	

// adding x axis label
var xLabel = svg2.append("text")
		.attr("x", width2 / 2)
		.attr("y", height2 + 50)
		.attr("font-size", "1.25em")
		.attr("text-anchor", "middle")
		.text("Time");

// adding y axis label
var yLabel = svg2.append("text")
		.attr("x", -150)
		.attr("y", -35)
		.attr("transform", "rotate(-90)")
		.attr("font-size", "1.25em")
		.attr("text-anchor", "middle")
		.text("Frequency");		
		
// loading data
d3.csv("2_Timeline_Data/partisan_timeline.csv", 

// formatting data
function(d){
  return { date : d3.timeParse("%Y-%m")(d.date), 
			All : d.All, 
			Republican : d.Republican,
			Democrat : d.Democrat};
},

// updating data based on drop down menu
function(data) {
	  
    // list of y variables and associated colors
    var freqGroup = ["All", "Republican", "Democrat"]
    var freqColors = ["#595AB7", "#259286", "#BD3613"]

    // adding the options to the button
    d3.select("#selectButton2")
		.selectAll('buttonOptions2')
			.data(freqGroup)
		.enter()
			.append('option')
		.text(function (d) { return d; }) // text showed in the menu
		.attr("value", function (d) { return d; }) // corresponding value returned by the button

    // setting color
    var freqColor = d3.scaleOrdinal()
		.domain(freqGroup)
		.range(freqColors);
	  
	// adding x axis
    var x = d3.scaleTime()
		.domain(d3.extent(data, function(d) { return d.date; }))
		.range([ 0, width2 ]);
    svg2.append("g")
		.attr("transform", "translate(0," + height2 + ")")
		.attr("class", "xAxis")
		.call(d3.axisBottom(x))
		.selectAll("text")
			.attr("transform", "translate(-10,0)rotate(-45)")
			.style("text-anchor", "end");
			
    // adding y axis
    var y = d3.scaleLinear()
		.domain([0, 165])
		.range([ height2, 0 ]);
    svg2.append("g")
		.attr("class", "yAxis")
		.call(d3.axisLeft(y));

	  
    // adding line to chart
    // initiating line
    var line = svg2
      .append('g')
      .append("path")
        .datum(data)
        .attr("d", d3.line()
          .x(function(d) { return x(+d.date) })
          .y(function(d) { return y(+d.All) })
        )
        .attr("stroke", function(d){ return freqColor("All") })
        .style("stroke-width", 4)
        .style("fill", "none")


	// adding hover interactivity
	function mouseover() {
		focus.style("opacity", 0.75)
		focusText.style("opacity",1)
	}
	
	var mousemove = function() {
		// recover the coordinates we need
		var x0 = x.invert(d3.mouse(this)[0]);
		var i = bisect(data, x0, 1);
		selectedData2 = data[i]
		focus
		.attr("cx", x(selectedData2.date))
		.attr("cy", y(selectedData2.All))
		focusText
		.html("Frequency:&nbsp;" + selectedData2.All)
		.attr("x", x(selectedData2.date))
		.attr("y", y(selectedData2.All)-35)
		}
	var mouseout = function() {
		focus.style("opacity", 0)
		focusText.style("opacity", 0)
	}	

	// finding x value of hover interactivity	
	var bisect = d3.bisector(function(d) { return d.date; }).left;
  
	// creating circle highlight of hover interactivity
	var focus = svg2
	.append('g')
	.append('circle')
		.style("fill", "#475B62")
		.attr("stroke", "black")
		.attr('r', 8.5)
		.style("opacity", 0)
		
	// creating coordinates of hover interactivity
	var focusText = svg2
	.append('g')
	.append('text')
		.style("opacity", 0)
		.attr("text-anchor", "middle")
		.attr("alignment-baseline", "middle")
		
	// setting svg of hover interactivity
	svg2
		.append('rect')
		.style("fill", "none")
		.style("pointer-events", "all")
		.attr('width', width2)
		.attr('height', height2)
		.on('mouseover', mouseover)
		.on('mousemove', mousemove)
		.on('mouseout', mouseout);		
	
  
    // updating chart
    function update2(selectedGroup2) {

    // filtering data
    var dataFilter2 = data.map(function(d){return {date: d.date, value: d[selectedGroup2]} })

	// removing previous svg of hover interactivity
	svg2
		.on('mouseover', null)
		.on('mousemove', null)
		.on('mouseout', null);	

	// readding updated coordinates	of hover interactivity	
	var mousemove = function() {
		var x0 = x.invert(d3.mouse(this)[0]);
		var i = bisect(dataFilter2, x0, 1);
		selectedData2 = dataFilter2[i]
		focus
		.attr("cx", x(selectedData2.date))
		.attr("cy", y(selectedData2.value))
		focusText
		.html("Frequency:&nbsp;" + selectedData2.value)
		.attr("x", x(selectedData2.date))
		.attr("y", y(selectedData2.value)-35)
		}
	var mouseout = function() {
		focus.style("opacity", 0)
		focusText.style("opacity", 0)
	}	

	// resetting updated svg of hover interactivity
	svg2
		.append('rect')
		.style("fill", "none")
		.style("pointer-events", "all")
		.attr('width', width2)
		.attr('height', height2)
		.on('mouseover', mouseover)
		.on('mousemove', mousemove)
		.on('mouseout', mouseout);	
		
    // readding updated line of chart
    line
        .datum(dataFilter2)
        .transition()
        .duration(1000)
		.attr("d", d3.line()
		.x(function(d) { return x(d.date) })
		.y(function(d) { return y(d.value) })
		)
		.attr("stroke", function(d){ return freqColor(selectedGroup2) })
	}

// setting drop down menu function
d3.select("#selectButton2").on("change", function(d) {
    // recover the option that has been chosen
    var selectedOption2 = d3.select(this).property("value")
    // run the updateChart function with this selected option
    update2(selectedOption2)
    })
  });
