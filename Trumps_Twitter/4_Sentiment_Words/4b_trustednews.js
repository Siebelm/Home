
// Load Data
var PositiveB = [{"word":"Enjoy","frequency":88,"sentiment":"Positive"},{"word":"Nice","frequency":12,"sentiment":"Positive"},{"word":"Wow","frequency":10,"sentiment":"Positive"},{"word":"Intelligence","frequency":7,"sentiment":"Positive"},{"word":"Win","frequency":7,"sentiment":"Positive"},{"word":"Congratulations","frequency":6,"sentiment":"Positive"},{"word":"Won","frequency":6,"sentiment":"Positive"},{"word":"Wonderful","frequency":6,"sentiment":"Positive"},{"word":"Approval","frequency":5,"sentiment":"Positive"},{"word":"Amazing","frequency":4,"sentiment":"Positive"},{"word":"Fairly","frequency":4,"sentiment":"Positive"},{"word":"Incredible","frequency":4,"sentiment":"Positive"},{"word":"Love","frequency":4,"sentiment":"Positive"},{"word":"Fantastic","frequency":3,"sentiment":"Positive"},{"word":"Led","frequency":3,"sentiment":"Positive"},{"word":"Positive","frequency":3,"sentiment":"Positive"},{"word":"Successful","frequency":3,"sentiment":"Positive"},{"word":"Top","frequency":3,"sentiment":"Positive"},{"word":"Tops","frequency":3,"sentiment":"Positive"},{"word":"Winning","frequency":3,"sentiment":"Positive"}];

var NegativeB = [{"word":"Fake","frequency":22,"sentiment":"Negative"},{"word":"Collusion","frequency":14,"sentiment":"Negative"},{"word":"Illegal","frequency":10,"sentiment":"Negative"},{"word":"Negative","frequency":9,"sentiment":"Negative"},{"word":"Witch","frequency":9,"sentiment":"Negative"},{"word":"Hard","frequency":7,"sentiment":"Negative"},{"word":"Hit","frequency":7,"sentiment":"Negative"},{"word":"Phony","frequency":7,"sentiment":"Negative"},{"word":"Bad","frequency":6,"sentiment":"Negative"},{"word":"Biased","frequency":6,"sentiment":"Negative"},{"word":"Corrupt","frequency":6,"sentiment":"Negative"},{"word":"Crooked","frequency":6,"sentiment":"Negative"},{"word":"Attack","frequency":5,"sentiment":"Negative"},{"word":"Crazy","frequency":5,"sentiment":"Negative"},{"word":"Failing","frequency":5,"sentiment":"Negative"},{"word":"Disgrace","frequency":4,"sentiment":"Negative"},{"word":"Dumb","frequency":4,"sentiment":"Negative"},{"word":"Hate","frequency":4,"sentiment":"Negative"},{"word":"Hoax","frequency":4,"sentiment":"Negative"},{"word":"Joke","frequency":4,"sentiment":"Negative"}]
;

// setting the dimensions and margins of the chart
var margin = { top: 20, right: 0, bottom: 30, left: 80 };
    width4b = 500 - margin.left - margin.right,
    height4b = 1070 - margin.top - margin.bottom;

// appending the svg object to the body of the page
var svg4b = d3.select("#_4b_trustednews")
	.append("svg")
	.attr("width", width4b + margin.left + margin.right)
	.attr("height", height4b + margin.top + margin.bottom)
	.append("g")
	.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// setting chart title
var Title = svg4b.append("text")
		.attr("y", -5)
		.attr("font-size", "1.75em")
		.attr("text-anchor", "right")
		.text("Top Words by Sentiment");

// adding X axis
var xb = d3.scaleLinear()
    .range([0, width4b]);
var xAxis4b = svg4b.append("g")
	.attr("transform", "translate(0," + height4b + ")")
	.attr("class", "xAxis")
  
// adding Y axis
var yb = d3.scaleBand()
    .range([0, height4b])
	.paddingInner(0.1)
	.paddingOuter(0.2);
var yAxis4b = svg4b.append("g")
	.attr("class", "yAxis")
  
// updating data (positive or negative data) based on button clicked
function update4B(data) {

		
// creating tooltip
var tip4b = d3.tip().attr("class", "d3-tip")
	.html(function(d){
		var text = "<strong>Tweets: <span style='color:steelblue'>"
					+ "Trusted News</strong></span><br>";
		text += "<strong>Sentiment: <span style='color:steelblue'>"
					+ d.sentiment + "</strong></span><br>";					
		text += "<strong>Word: <span style='color:steelblue'>"
					+ d.word + "</strong></span><br>";
		text += "<strong>Frequency: <span style='color:steelblue'>"
					+ d.frequency + "</strong></span>";
		return text;
	})	

svg4b.call(tip4b);		

	// updating the X axis
	xb.domain([0, 90])
	xAxis4b.call(d3.axisBottom(xb))
	.selectAll("text")
      .attr("transform", "translate(-10,0)rotate(-45)")
      .style("text-anchor", "end");
	  
	// updating the Y axis
	yb.domain(data.map(function(d) { return d.word; }))
	yAxis4b.transition().duration(1000).call(d3.axisLeft(yb));
	
	// adding bars to chart
	var bars4b = svg4b.selectAll(".bars4b")
		.data(data);

	// initiating bars
	bars4b
		.enter()
		.append("rect")
		.attr("class", "bars4b")
		.attr("y", function(d) { return yb(d.word); })
		.attr("height", yb.bandwidth() )
		.attr("x", 0)
		.attr("width", function(d) { return xb(d.frequency); })
		.on("mouseover", function(d,i){
			tip4b.show(d)	
			tempColor = this.style.fill;
			d3.select(this)
				.transition() //takes a little longer to change the color
				.style('opacity', 0.5)
				.style('fill', 'grey')
			})
		.on("mouseout", function(d,i){
			tip4b.hide(d)
				d3.select(this)
					.transition().duration(500) //default is 250
					.style('opacity', 1)
					.style('fill', tempColor)
			})

	// removing bars
	bars4b
		.enter()
		.append("rect")
		.merge(bars4b)
		.attr("x", 0 )
		.attr("width", function(d) { return 0; })
			
	// Animation (readd bars)
	bars4b
		.enter()
		.append("rect")
		.merge(bars4b)
		.transition()
		.duration(100)
		.attr("x", 0 )
		.attr("width", function(d) { return xb(d.frequency); })
		.delay(function(d,i){return(i*100)})		
		
		
	bars4b
		.exit()
		.remove()
}

// Initialize the plot with the first dataset
update4B(NegativeB);