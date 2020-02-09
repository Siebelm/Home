
// Load Data
var PositiveA = [{"word":"Enjoy","frequency":113,"sentiment":"Positive"},{"word":"Wow","frequency":38,"sentiment":"Positive"},{"word":"Win","frequency":25,"sentiment":"Positive"},{"word":"Nice","frequency":20,"sentiment":"Positive"},{"word":"Won","frequency":20,"sentiment":"Positive"},{"word":"Wonderful","frequency":15,"sentiment":"Positive"},{"word":"Intelligence","frequency":14,"sentiment":"Positive"},{"word":"Love","frequency":13,"sentiment":"Positive"},{"word":"Victory","frequency":13,"sentiment":"Positive"},{"word":"Positive","frequency":12,"sentiment":"Positive"},{"word":"Congratulations","frequency":11,"sentiment":"Positive"},{"word":"Correct","frequency":10,"sentiment":"Positive"},{"word":"Success","frequency":10,"sentiment":"Positive"},{"word":"Approval","frequency":9,"sentiment":"Positive"},{"word":"Respect","frequency":9,"sentiment":"Positive"},{"word":"Top","frequency":9,"sentiment":"Positive"},{"word":"Winning","frequency":9,"sentiment":"Positive"},{"word":"Fantastic","frequency":8,"sentiment":"Positive"},{"word":"Happy","frequency":8,"sentiment":"Positive"},{"word":"Tough","frequency":8,"sentiment":"Positive"}]
;

var NegativeA = [{"word":"Fake","frequency":302,"sentiment":"Negative"},{"word":"Dishonest","frequency":99,"sentiment":"Negative"},{"word":"Failing","frequency":94,"sentiment":"Negative"},{"word":"Bad","frequency":73,"sentiment":"Negative"},{"word":"Phony","frequency":42,"sentiment":"Negative"},{"word":"Wrong","frequency":41,"sentiment":"Negative"},{"word":"Biased","frequency":35,"sentiment":"Negative"},{"word":"Crooked","frequency":35,"sentiment":"Negative"},{"word":"Witch","frequency":34,"sentiment":"Negative"},{"word":"Crazy","frequency":31,"sentiment":"Negative"},{"word":"Hard","frequency":31,"sentiment":"Negative"},{"word":"Collusion","frequency":29,"sentiment":"Negative"},{"word":"Hit","frequency":25,"sentiment":"Negative"},{"word":"Negative","frequency":24,"sentiment":"Negative"},{"word":"Illegal","frequency":22,"sentiment":"Negative"},{"word":"False","frequency":21,"sentiment":"Negative"},{"word":"Worse","frequency":21,"sentiment":"Negative"},{"word":"Corrupt","frequency":20,"sentiment":"Negative"},{"word":"Lost","frequency":19,"sentiment":"Negative"},{"word":"Sad","frequency":19,"sentiment":"Negative"}];

// setting the dimensions and margins of the chart
var margin = { top: 20, right: 0, bottom: 30, left: 80 };
    width4a = 500 - margin.left - margin.right,
    height4a = 1070 - margin.top - margin.bottom;

// appending the svg object to the body of the page
var svg4a = d3.select("#_4a_news")
	.append("svg")
	.attr("width", width4a + margin.left + margin.right)
	.attr("height", height4a + margin.top + margin.bottom)
	.append("g")
	.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// setting chart title
var Title = svg4a.append("text")
		.attr("y", -5)
		.attr("font-size", "1.75em")
		.attr("text-anchor", "right")
		.text("Top Words by Sentiment");
		
// creating tooltip
var tip4a = d3.tip().attr("class", "d3-tip")
	.html(function(d){
		var text = "<strong>Tweets: <span style='color:steelblue'>"
					+ "All News</strong></span><br>";
		text += "<strong>Sentiment: <span style='color:steelblue'>"
					+ d.sentiment + "</strong></span><br>";	
		text += "<strong>Word: <span style='color:steelblue'>"
					+ d.word + "</strong></span><br>";
		text += "<strong>Frequency: <span style='color:steelblue'>"
					+ d.frequency + "</strong></span>";
		return text;
	})		

svg4a.call(tip4a);		

// adding x axis
var xa = d3.scaleLinear()
    .range([0, width4a]);
var xAxis4a = svg4a.append("g")
	.attr("transform", "translate(0," + height4a + ")")
	.attr("class", "xAxis")
  
// adding y axis
var ya = d3.scaleBand()
    .range([0, height4a])
	.paddingInner(0.1)
	.paddingOuter(0.2);
var yAxis4a = svg4a.append("g")
	.attr("class", "yAxis")
  
// updating data (positive or negative data) based on button clicked
function update4A(data) {
		
	// updating the X axis
	xa.domain([0, 302])
	xAxis4a.call(d3.axisBottom(xa))
	.selectAll("text")
      .attr("transform", "translate(-10,0)rotate(-45)")
      .style("text-anchor", "end");
	  
	// updating the Y axis
	ya.domain(data.map(function(d) { return d.word; }))
	yAxis4a.transition().duration(1000).call(d3.axisLeft(ya));
	
	// adding bars to chart
	var bars4a = svg4a.selectAll(".bars4a")
		.data(data);

	// initiating bars
	bars4a
		.enter()
		.append("rect")
		.attr("class", "bars4a")
		.attr("y", function(d) { return ya(d.word); })
		.attr("height", ya.bandwidth() )
		.attr("x", 0)
		.attr("width", function(d) { return xa(d.frequency); })
		.on("mouseover", function(d){
			tip4a.show(d)	
			tempColor = this.style.fill;
			d3.select(this)
				.transition() //takes a little longer to change the color
				.style('opacity', 0.5)
				.style('fill', 'grey')
			})
		.on("mouseout", function(d){
			tip4a.hide(d)
				d3.select(this)
					.transition().duration(500) 
					.style('opacity', 1)
					.style('fill', tempColor)
			})

	// removing bars
	bars4a
		.enter()
		.append("rect")
		.merge(bars4a)
		.attr("x", 0 )
		.attr("width", function(d) { return 0; })
			
	// animation (readd bars)
	bars4a
		.enter()
		.append("rect")
		.merge(bars4a)
		.transition()
		.duration(100)
		.attr("x", 0 )
		.attr("width", function(d) { return xa(d.frequency); })
		.delay(function(d,i){return(i*100)})		
		
		
	bars4a
		.exit()
		.remove()
}

// Initialize the plot with the first dataset
update4A(NegativeA);