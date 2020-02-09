
// Load Data
var PositiveD = [{"word":"Strong","frequency":54,"sentiment":"Positive"},{"word":"Endorsement","frequency":34,"sentiment":"Positive"},{"word":"Loves","frequency":20,"sentiment":"Positive"},{"word":"Protect","frequency":20,"sentiment":"Positive"},{"word":"Tough","frequency":18,"sentiment":"Positive"},{"word":"Safe","frequency":17,"sentiment":"Positive"},{"word":"Win","frequency":17,"sentiment":"Positive"},{"word":"Merit","frequency":14,"sentiment":"Positive"},{"word":"Secure","frequency":12,"sentiment":"Positive"},{"word":"Amazing","frequency":9,"sentiment":"Positive"},{"word":"Incredible","frequency":9,"sentiment":"Positive"},{"word":"Fantastic","frequency":8,"sentiment":"Positive"},{"word":"Fast","frequency":8,"sentiment":"Positive"},{"word":"Ready","frequency":8,"sentiment":"Positive"},{"word":"Tougher","frequency":7,"sentiment":"Positive"},{"word":"Happy","frequency":6,"sentiment":"Positive"},{"word":"Properly","frequency":6,"sentiment":"Positive"},{"word":"Protection","frequency":6,"sentiment":"Positive"},{"word":"Proud","frequency":6,"sentiment":"Positive"},{"word":"Respect","frequency":6,"sentiment":"Positive"}];

var NegativeD = [{"word":"Crime","frequency":77,"sentiment":"Negative"},{"word":"Illegal","frequency":72,"sentiment":"Negative"},{"word":"Weak","frequency":31,"sentiment":"Negative"},{"word":"Gang","frequency":27,"sentiment":"Negative"},{"word":"Hard","frequency":26,"sentiment":"Negative"},{"word":"Criminal","frequency":23,"sentiment":"Negative"},{"word":"Bad","frequency":22,"sentiment":"Negative"},{"word":"Dangerous","frequency":19,"sentiment":"Negative"},{"word":"Fake","frequency":18,"sentiment":"Negative"},{"word":"Crooked","frequency":15,"sentiment":"Negative"},{"word":"Desperately","frequency":14,"sentiment":"Negative"},{"word":"Ridiculous","frequency":13,"sentiment":"Negative"},{"word":"Terrible","frequency":12,"sentiment":"Negative"},{"word":"Gangs","frequency":11,"sentiment":"Negative"},{"word":"Illegally","frequency":11,"sentiment":"Negative"},{"word":"Badly","frequency":10,"sentiment":"Negative"},{"word":"Worst","frequency":10,"sentiment":"Negative"},{"word":"Witch","frequency":8,"sentiment":"Negative"},{"word":"Fault","frequency":7,"sentiment":"Negative"},{"word":"Killed","frequency":7,"sentiment":"Negative"}];

// setting the dimensions and margins of the chart
var margin = { top: 20, right: 0, bottom: 30, left: 80 };
    width4d = 500 - margin.left - margin.right,
    height4d = 1070 - margin.top - margin.bottom;

// appending the svg object to the body of the page
var svg4d = d3.select("#_4d_immigration")
	.append("svg")
	.attr("width", width4d + margin.left + margin.right)
	.attr("height", height4d + margin.top + margin.bottom)
	.append("g")
	.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// setting chart title
var Title = svg4d.append("text")
		.attr("y", -5)
		.attr("font-size", "1.75em")
		.attr("text-anchor", "right")
		.text("Top Words by Sentiment");
		
// creating tooltip
var tip4d = d3.tip().attr("class", "d3-tip")
	.html(function(d){
		var text = "<strong>Tweets: <span style='color:steelblue'>"
					+ "Immigration Policy</strong></span><br>";
		text += "<strong>Sentiment: <span style='color:steelblue'>"
					+ d.sentiment + "</strong></span><br>";	
		text += "<strong>Word: <span style='color:steelblue'>"
					+ d.word + "</strong></span><br>";
		text += "<strong>Frequency: <span style='color:steelblue'>"
					+ d.frequency + "</strong></span>";
		return text;
	})		

svg4d.call(tip4d);		

// adding X axis
var xd = d3.scaleLinear()
    .range([0, width4d]);
var xAxis4d = svg4d.append("g")
	.attr("transform", "translate(0," + height4d + ")")
	.attr("class", "xAxis")
  
// adding Y axis
var yd = d3.scaleBand()
    .range([0, height4d])
	.paddingInner(0.1)
	.paddingOuter(0.2);
var yAxis4d = svg4d.append("g")
	.attr("class", "yAxis")
  
// updating data (positive or negative data) based on button clicked
function update4D(data) {
		
	// updating the X axis
	xd.domain([0, 130])
	xAxis4d.call(d3.axisBottom(xd))
	.selectAll("text")
      .attr("transform", "translate(-10,0)rotate(-45)")
      .style("text-anchor", "end");
	  
	// updating the Y axis
	yd.domain(data.map(function(d) { return d.word; }))
	yAxis4d.transition().duration(1000).call(d3.axisLeft(yd));
	
	// adding bars to chart
	var bars4d = svg4d.selectAll(".bars4d")
		.data(data);

	// initiating bars
	bars4d
		.enter()
		.append("rect")
		.attr("class", "bars4d")
		.attr("y", function(d) { return yd(d.word); })
		.attr("height", yd.bandwidth() )
		.attr("x", 0)
		.attr("width", function(d) { return xd(d.frequency); })
		.on("mouseover", function(d,i){
			tip4d.show(d)	
			tempColor = this.style.fill;
			d3.select(this)
				.transition() //takes a little longer to change the color
				.style('opacity', 0.5)
				.style('fill', 'grey')
			})
		.on("mouseout", function(d,i){
			tip4d.hide(d)
				d3.select(this)
					.transition().duration(500) //default is 250
					.style('opacity', 1)
					.style('fill', tempColor)
			})

	// removing bars
	bars4d
		.enter()
		.append("rect")
		.merge(bars4d)
		.attr("x", 0 )
		.attr("width", function(d) { return 0; })
			
	// Animation (readd bars)
	bars4d
		.enter()
		.append("rect")
		.merge(bars4d)
		.transition()
		.duration(100)
		.attr("x", 0 )
		.attr("width", function(d) { return xd(d.frequency); })
		.delay(function(d,i){return(i*100)})		
		
		
	bars4d
		.exit()
		.remove()
}

// Initialize the plot with the first dataset
update4D(NegativeD);