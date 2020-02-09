
// Load Data
var PositiveC = [{"word":"Win","frequency":62,"sentiment":"Positive"},{"word":"Strong","frequency":37,"sentiment":"Positive"},{"word":"Won","frequency":25,"sentiment":"Positive"},{"word":"Wow","frequency":25,"sentiment":"Positive"},{"word":"Supreme","frequency":19,"sentiment":"Positive"},{"word":"Tough","frequency":19,"sentiment":"Positive"},{"word":"Protect","frequency":16,"sentiment":"Positive"},{"word":"Fast","frequency":15,"sentiment":"Positive"},{"word":"Lead","frequency":14,"sentiment":"Positive"},{"word":"Top","frequency":13,"sentiment":"Positive"},{"word":"Congratulations","frequency":11,"sentiment":"Positive"},{"word":"Approve","frequency":10,"sentiment":"Positive"},{"word":"Led","frequency":10,"sentiment":"Positive"},{"word":"Ready","frequency":10,"sentiment":"Positive"},{"word":"Defeat","frequency":9,"sentiment":"Positive"},{"word":"Endorsement","frequency":9,"sentiment":"Positive"},{"word":"Proven","frequency":9,"sentiment":"Positive"},{"word":"Victory","frequency":9,"sentiment":"Positive"},{"word":"Easy","frequency":8,"sentiment":"Positive"},{"word":"Favor","frequency":8,"sentiment":"Positive"}];

var NegativeC = [{"word":"Crooked","frequency":289,"sentiment":"Negative"},{"word":"Bad","frequency":106,"sentiment":"Negative"},{"word":"Fake","frequency":66,"sentiment":"Negative"},{"word":"Witch","frequency":65,"sentiment":"Negative"},{"word":"Collusion","frequency":64,"sentiment":"Negative"},{"word":"Crime","frequency":62,"sentiment":"Negative"},{"word":"Illegal","frequency":54,"sentiment":"Negative"},{"word":"Angry","frequency":51,"sentiment":"Negative"},{"word":"Hard","frequency":41,"sentiment":"Negative"},{"word":"Weak","frequency":40,"sentiment":"Negative"},{"word":"Phony","frequency":37,"sentiment":"Negative"},{"word":"Crazy","frequency":26,"sentiment":"Negative"},{"word":"Dishonest","frequency":26,"sentiment":"Negative"},{"word":"Hoax","frequency":25,"sentiment":"Negative"},{"word":"Terrible","frequency":25,"sentiment":"Negative"},{"word":"Corrupt","frequency":24,"sentiment":"Negative"},{"word":"Lost","frequency":24,"sentiment":"Negative"},{"word":"Goofy","frequency":23,"sentiment":"Negative"},{"word":"Worst","frequency":23,"sentiment":"Negative"},{"word":"Disaster","frequency":22,"sentiment":"Negative"}];

// setting the dimensions and margins of the chart
var margin = { top: 20, right: 0, bottom: 30, left: 80 };
    width4c = 500 - margin.left - margin.right,
    height4c = 1070 - margin.top - margin.bottom;

// appending the svg object to the body of the page
var svg4c = d3.select("#_4c_democrat")
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
					+ "Democrat Tweets</strong></span><br>";
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
	xc.domain([0, 300])
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