
// Load Data
var PositiveA = [{"word":"Strong","frequency":113,"sentiment":"Positive"},{"word":"Endorsement","frequency":70,"sentiment":"Positive"},{"word":"Win","frequency":62,"sentiment":"Positive"},{"word":"Honor","frequency":50,"sentiment":"Positive"},{"word":"Reform","frequency":48,"sentiment":"Positive"},{"word":"Tough","frequency":45,"sentiment":"Positive"},{"word":"Fantastic","frequency":44,"sentiment":"Positive"},{"word":"Congratulations","frequency":42,"sentiment":"Positive"},{"word":"Fair","frequency":42,"sentiment":"Positive"},{"word":"Protect","frequency":42,"sentiment":"Positive"},{"word":"Wonderful","frequency":40,"sentiment":"Positive"},{"word":"Incredible","frequency":38,"sentiment":"Positive"},{"word":"Loves","frequency":37,"sentiment":"Positive"},{"word":"Happy","frequency":30,"sentiment":"Positive"},{"word":"Nice","frequency":29,"sentiment":"Positive"},{"word":"Beautiful","frequency":28,"sentiment":"Positive"},{"word":"Love","frequency":28,"sentiment":"Positive"},{"word":"Amazing","frequency":27,"sentiment":"Positive"},{"word":"Fast","frequency":27,"sentiment":"Positive"},{"word":"Proud","frequency":27,"sentiment":"Positive"}];
;

var NegativeA = [{"word":"Crime","frequency":129,"sentiment":"Negative"},{"word":"Hard","frequency":97,"sentiment":"Negative"},{"word":"Bad","frequency":82,"sentiment":"Negative"},{"word":"Illegal","frequency":79,"sentiment":"Negative"},{"word":"Fake","frequency":75,"sentiment":"Negative"},{"word":"Crooked","frequency":54,"sentiment":"Negative"},{"word":"Weak","frequency":46,"sentiment":"Negative"},{"word":"Disaster","frequency":34,"sentiment":"Negative"},{"word":"Terrible","frequency":32,"sentiment":"Negative"},{"word":"Worst","frequency":30,"sentiment":"Negative"},{"word":"Attack","frequency":29,"sentiment":"Negative"},{"word":"Criminal","frequency":28,"sentiment":"Negative"},{"word":"Gang","frequency":27,"sentiment":"Negative"},{"word":"Hit","frequency":27,"sentiment":"Negative"},{"word":"Witch","frequency":24,"sentiment":"Negative"},{"word":"Badly","frequency":23,"sentiment":"Negative"},{"word":"Dangerous","frequency":22,"sentiment":"Negative"},{"word":"Lost","frequency":21,"sentiment":"Negative"},{"word":"Horrible","frequency":20,"sentiment":"Negative"},{"word":"Dead","frequency":19,"sentiment":"Negative"}];

// setting the dimensions and margins of the chart
var margin = { top: 20, right: 0, bottom: 30, left: 80 };
    width4a = 500 - margin.left - margin.right,
    height4a = 1070 - margin.top - margin.bottom;

// appending the svg object to the body of the page
var svg4a = d3.select("#_4a_policy")
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
					+ "All Policy</strong></span><br>";
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
	xa.domain([0, 130])
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