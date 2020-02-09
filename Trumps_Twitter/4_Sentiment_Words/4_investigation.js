
// Load Data
var PositiveA = [{"word":"Strong","frequency":71,"sentiment":"Positive"},{"word":"Supreme","frequency":60,"sentiment":"Positive"},{"word":"Endorsement","frequency":54,"sentiment":"Positive"},{"word":"Loves","frequency":49,"sentiment":"Positive"},{"word":"Win","frequency":39,"sentiment":"Positive"},{"word":"Tough","frequency":32,"sentiment":"Positive"},{"word":"Wow","frequency":28,"sentiment":"Positive"},{"word":"Top","frequency":24,"sentiment":"Positive"},{"word":"Intelligence","frequency":22,"sentiment":"Positive"},{"word":"Fantastic","frequency":19,"sentiment":"Positive"},{"word":"Congratulations","frequency":15,"sentiment":"Positive"},{"word":"Lover","frequency":15,"sentiment":"Positive"},{"word":"Nice","frequency":12,"sentiment":"Positive"},{"word":"Wonderful","frequency":12,"sentiment":"Positive"},{"word":"Happy","frequency":11,"sentiment":"Positive"},{"word":"Honor","frequency":11,"sentiment":"Positive"},{"word":"Protect","frequency":11,"sentiment":"Positive"},{"word":"Led","frequency":10,"sentiment":"Positive"},{"word":"Reform","frequency":9,"sentiment":"Positive"},{"word":"Free","frequency":8,"sentiment":"Positive"}];

var NegativeA = [{"word":"Crime","frequency":189,"sentiment":"Negative"},{"word":"Witch","frequency":143,"sentiment":"Negative"},{"word":"Collusion","frequency":131,"sentiment":"Negative"},{"word":"Fake","frequency":110,"sentiment":"Negative"},{"word":"Crooked","frequency":101,"sentiment":"Negative"},{"word":"Bad","frequency":59,"sentiment":"Negative"},{"word":"Illegal","frequency":56,"sentiment":"Negative"},{"word":"Phony","frequency":51,"sentiment":"Negative"},{"word":"Hoax","frequency":46,"sentiment":"Negative"},{"word":"Angry","frequency":45,"sentiment":"Negative"},{"word":"Criminal","frequency":43,"sentiment":"Negative"},{"word":"Obstruction","frequency":38,"sentiment":"Negative"},{"word":"Weak","frequency":34,"sentiment":"Negative"},{"word":"Hard","frequency":32,"sentiment":"Negative"},{"word":"Corrupt","frequency":25,"sentiment":"Negative"},{"word":"Dishonest","frequency":25,"sentiment":"Negative"},{"word":"Conflicted","frequency":24,"sentiment":"Negative"},{"word":"Corruption","frequency":24,"sentiment":"Negative"},{"word":"Lies","frequency":23,"sentiment":"Negative"},{"word":"Terrible","frequency":23,"sentiment":"Negative"}];

// setting the dimensions and margins of the chart
var margin = { top: 20, right: 0, bottom: 30, left: 80 };
    width4a = 500 - margin.left - margin.right,
    height4a = 1070 - margin.top - margin.bottom;

// appending the svg object to the body of the page
var svg4a = d3.select("#_4_investigation")
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
					+ "Investigation Tweets</strong></span><br>";
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
	xa.domain([0, 190])
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