
// Load Data
var PositiveC = [{"word":"Wow","frequency":34,"sentiment":"Positive"},{"word":"Enjoy","frequency":30,"sentiment":"Positive"},{"word":"Win","frequency":19,"sentiment":"Positive"},{"word":"Won","frequency":16,"sentiment":"Positive"},{"word":"Victory","frequency":12,"sentiment":"Positive"},{"word":"Love","frequency":10,"sentiment":"Positive"},{"word":"Nice","frequency":10,"sentiment":"Positive"},{"word":"Positive","frequency":10,"sentiment":"Positive"},{"word":"Success","frequency":9,"sentiment":"Positive"},{"word":"Wonderful","frequency":9,"sentiment":"Positive"},{"word":"Correct","frequency":8,"sentiment":"Positive"},{"word":"Winning","frequency":8,"sentiment":"Positive"},{"word":"Fair","frequency":7,"sentiment":"Positive"},{"word":"Happy","frequency":7,"sentiment":"Positive"},{"word":"Intelligence","frequency":7,"sentiment":"Positive"},{"word":"Powerful","frequency":7,"sentiment":"Positive"},{"word":"Respect","frequency":7,"sentiment":"Positive"},{"word":"Congratulations","frequency":6,"sentiment":"Positive"},{"word":"Fantastic","frequency":6,"sentiment":"Positive"},{"word":"Lead","frequency":6,"sentiment":"Positive"}]
;

var NegativeC = [{"word":"Fake","frequency":297,"sentiment":"Negative"},{"word":"Dishonest","frequency":99,"sentiment":"Negative"},{"word":"Failing","frequency":94,"sentiment":"Negative"},{"word":"Bad","frequency":70,"sentiment":"Negative"},{"word":"Wrong","frequency":38,"sentiment":"Negative"},{"word":"Phony","frequency":37,"sentiment":"Negative"},{"word":"Biased","frequency":35,"sentiment":"Negative"},{"word":"Crooked","frequency":30,"sentiment":"Negative"},{"word":"Crazy","frequency":28,"sentiment":"Negative"},{"word":"Witch","frequency":28,"sentiment":"Negative"},{"word":"Hard","frequency":24,"sentiment":"Negative"},{"word":"Negative","frequency":23,"sentiment":"Negative"},{"word":"False","frequency":20,"sentiment":"Negative"},{"word":"Hit","frequency":20,"sentiment":"Negative"},{"word":"Enemy","frequency":18,"sentiment":"Negative"},{"word":"Sad","frequency":18,"sentiment":"Negative"},{"word":"Worse","frequency":18,"sentiment":"Negative"},{"word":"Lost","frequency":17,"sentiment":"Negative"},{"word":"Collusion","frequency":16,"sentiment":"Negative"},{"word":"Corrupt","frequency":15,"sentiment":"Negative"}]
;

// setting the dimensions and margins of the chart
var margin = { top: 20, right: 0, bottom: 30, left: 80 };
    width4c = 500 - margin.left - margin.right,
    height4c = 1070 - margin.top - margin.bottom;

// appending the svg object to the body of the page
var svg4c = d3.select("#_4c_fakenews")
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
					+ "Fake News</strong></span><br>";
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