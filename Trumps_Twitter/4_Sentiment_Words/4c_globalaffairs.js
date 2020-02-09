
// Load Data
var PositiveC = [{"word":"Honor","frequency":35,"sentiment":"Positive"},{"word":"Wonderful","frequency":25,"sentiment":"Positive"},{"word":"Strong","frequency":18,"sentiment":"Positive"},{"word":"Fair","frequency":16,"sentiment":"Positive"},{"word":"Peace","frequency":14,"sentiment":"Positive"},{"word":"Nice","frequency":13,"sentiment":"Positive"},{"word":"Progress","frequency":13,"sentiment":"Positive"},{"word":"Congratulations","frequency":12,"sentiment":"Positive"},{"word":"Win","frequency":12,"sentiment":"Positive"},{"word":"Tough","frequency":11,"sentiment":"Positive"},{"word":"Advantage","frequency":10,"sentiment":"Positive"},{"word":"Beautiful","frequency":10,"sentiment":"Positive"},{"word":"Love","frequency":10,"sentiment":"Positive"},{"word":"Success","frequency":10,"sentiment":"Positive"},{"word":"Incredible","frequency":9,"sentiment":"Positive"},{"word":"Commitment","frequency":8,"sentiment":"Positive"},{"word":"Fast","frequency":8,"sentiment":"Positive"},{"word":"Honored","frequency":8,"sentiment":"Positive"},{"word":"Protection","frequency":8,"sentiment":"Positive"},{"word":"Ready","frequency":8,"sentiment":"Positive"}];

var NegativeC = [{"word":"Fake","frequency":25,"sentiment":"Negative"},{"word":"Hard","frequency":24,"sentiment":"Negative"},{"word":"Attack","frequency":22,"sentiment":"Negative"},{"word":"Bad","frequency":21,"sentiment":"Negative"},{"word":"Crime","frequency":11,"sentiment":"Negative"},{"word":"Weak","frequency":11,"sentiment":"Negative"},{"word":"Terror","frequency":10,"sentiment":"Negative"},{"word":"Illegal","frequency":9,"sentiment":"Negative"},{"word":"Dishonest","frequency":8,"sentiment":"Negative"},{"word":"Horrible","frequency":8,"sentiment":"Negative"},{"word":"Wrong","frequency":8,"sentiment":"Negative"},{"word":"Crooked","frequency":7,"sentiment":"Negative"},{"word":"Killed","frequency":7,"sentiment":"Negative"},{"word":"Killing","frequency":7,"sentiment":"Negative"},{"word":"Lost","frequency":7,"sentiment":"Negative"},{"word":"Dangerous","frequency":6,"sentiment":"Negative"},{"word":"Failed","frequency":6,"sentiment":"Negative"},{"word":"Failing","frequency":6,"sentiment":"Negative"},{"word":"Hostage","frequency":6,"sentiment":"Negative"},{"word":"Terrible","frequency":6,"sentiment":"Negative"}];

// setting the dimensions and margins of the chart
var margin = { top: 20, right: 0, bottom: 30, left: 80 };
    width4c = 500 - margin.left - margin.right,
    height4c = 1070 - margin.top - margin.bottom;

// appending the svg object to the body of the page
var svg4c = d3.select("#_4c_globalaffairs")
	.append("svg")
	.attr("width", width4c + margin.left + margin.right)
	.attr("height", height4c + margin.top + margin.bottom)
	.append("g")
	.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// setting chart title
var Title = svg4c.append("text")
		.attr("y", -5)
		.attr("font-size", "1.75em")
		.attr("text-anchor", "right")
		.text("Top Words by Sentiment");
		
// creating tooltip
var tip4c = d3.tip().attr("class", "d3-tip")
	.html(function(d){
		var text = "<strong>Tweets: <span style='color:steelblue'>"
					+ "Global Policy</strong></span><br>";
		text += "<strong>Sentiment: <span style='color:steelblue'>"
					+ d.sentiment + "</strong></span><br>";	
		text += "<strong>Word: <span style='color:steelblue'>"
					+ d.word + "</strong></span><br>";
		text += "<strong>Frequency: <span style='color:steelblue'>"
					+ d.frequency + "</strong></span>";
		return text;
	})		

svg4c.call(tip4c);		

// adding X axis
var xc = d3.scaleLinear()
    .range([0, width4c]);
var xAxis4c = svg4c.append("g")
	.attr("transform", "translate(0," + height4c + ")")
	.attr("class", "xAxis")
  
// adding Y axis
var yc = d3.scaleBand()
    .range([0, height4c])
	.paddingInner(0.1)
	.paddingOuter(0.2);
var yAxis4c = svg4c.append("g")
	.attr("class", "yAxis")
  
// updating data (positive or negative data) based on button clicked
function update4C(data) {
		
	// updating the X axis
	xc.domain([0, 130])
	xAxis4c.call(d3.axisBottom(xc))
	.selectAll("text")
      .attr("transform", "translate(-10,0)rotate(-45)")
      .style("text-anchor", "end");
	  
	// updating the Y axis
	yc.domain(data.map(function(d) { return d.word; }))
	yAxis4c.transition().duration(1000).call(d3.axisLeft(yc));
	
	// adding bars to chart
	var bars4c = svg4c.selectAll(".bars4c")
		.data(data);

	// initiating bars
	bars4c
		.enter()
		.append("rect")
		.attr("class", "bars4c")
		.attr("y", function(d) { return yc(d.word); })
		.attr("height", yc.bandwidth() )
		.attr("x", 0)
		.attr("width", function(d) { return xc(d.frequency); })
		.on("mouseover", function(d,i){
			tip4c.show(d)	
			tempColor = this.style.fill;
			d3.select(this)
				.transition() //takes a little longer to change the color
				.style('opacity', 0.5)
				.style('fill', 'grey')
			})
		.on("mouseout", function(d,i){
			tip4c.hide(d)
				d3.select(this)
					.transition().duration(500) //default is 250
					.style('opacity', 1)
					.style('fill', tempColor)
			})

	// removing bars
	bars4c
		.enter()
		.append("rect")
		.merge(bars4c)
		.attr("x", 0 )
		.attr("width", function(d) { return 0; })
			
	// Animation (readd bars)
	bars4c
		.enter()
		.append("rect")
		.merge(bars4c)
		.transition()
		.duration(100)
		.attr("x", 0 )
		.attr("width", function(d) { return xc(d.frequency); })
		.delay(function(d,i){return(i*100)})		
		
		
	bars4c
		.exit()
		.remove()
}

// Initialize the plot with the first dataset
update4C(NegativeC);