
// loading data
var All1 = [{
				"sentiment":"Negative","dict":"2 news","freq":807,"prop":61.2,"rt":16565,"fav":59547
			},
			{
				"sentiment":"Positive","dict":"2 news","freq":511,"prop":38.8,"rt":9981,"fav":39424
    }];
var Trusted1 = [{
				"sentiment":"Negative","dict":"4 trustednews","freq":174,"prop":39.1,"rt":14051,"fav":42559
				},
				{
				"sentiment":"Positive","dict":"4 trustednews","freq":271,"prop":60.9,"rt":8926,"fav":35192
    }];
var Fake1 = [{
				"sentiment":"Negative","dict":"3 fakenews","freq":633,"prop":72.5,"rt":17256,"fav":64217
				},
				{
				"sentiment":"Positive","dict":"3 fakenews","freq":240,"prop":27.5,"rt":11171,"fav":44204
    }];	

// setting the dimensions and margins of the chart
var margin = { top: 20, right: 0, bottom: 30, left: 80 };

var width1 = 500 - margin.left - margin.right,
    height1 = 200 - margin.top - margin.bottom;

// appending the svg object to the body of the page			
var svg1 = d3.select("#_1_news")
	.append("svg")
    .attr("width", width1 + margin.left + margin.right)
    .attr("height", height1 + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

// adding title
var Title = svg1.append("text")
		.attr("y", -5)
		.attr("font-size", "1.75em")
		.attr("text-anchor", "left")
		.text("Sentiment of News Media");
			
		
// creating tooltip
var tip1 = d3.tip().attr("class", "d3-tip")
	.html(function(d){
		var text = "<strong>Sentiment: <span style='color:#ca2c92'>"
					+ d.sentiment + "</strong></span><br>";
		text += "<strong>Proportion: <span style='color:#ca2c92;text-transform:capitalize;'>"
					+ d.prop + "%</strong></span><br>";
		text += "<strong>Frequency: <span style='color:#ca2c92'>"
					+ d.freq + "</strong></span><br>";
		text += "<strong>Retweets: <span style='color:#ca2c92'>"
					+ d.rt + "</strong></span><br>";
		text += "<strong>Favorites: <span style='color:#ca2c92'>"
					+ d.fav + "</strong></span>";
		return text;
	})			

svg1.call(tip1);	

// adding x axis
var x = d3.scaleLinear()
    .range([0, width1]);
	
// adding y axis
var y = d3.scaleBand()
    .range([0, height1])
	.paddingInner(0.1)
	.paddingOuter(0.2);
var yAxis1 = svg1.append("g")
	.attr("class", "yAxis")

// updating data
function update1(data) {
		
	// updating the X axis
	x.domain([0, 75])

	// updating the Y axis
	y.domain(data.map(function(d) { return d.sentiment; }))
	yAxis1.transition().duration(1000)
		.call(d3.axisLeft().scale(y))
		.selectAll("text")
			.style("text-anchor", "end")
			.attr("font-size", "1.4em")
	
	var bars1 = svg1.selectAll(".bars1")
		.data(data);		
				
	// initiating bars
	bars1		
		.enter()
		.append("rect")
		.attr("class", "bars1")
		.attr("y", function(d) { return y(d.sentiment); })
		.attr("height", y.bandwidth() )
		.attr("x", 0)
		.attr("width", function(d) { return x(d.prop); })
		.on("mouseover", function(d){
			tip1.show(d)	
			tempColor = this.style.fill;
			d3.select(this)
				.transition() 
				.style('opacity', 0.5)
				.style('fill', 'grey')
			})
		.on("mouseout", function(d){
			tip1.hide(d)
				d3.select(this)
					.transition().duration(500) 
					.style('opacity', 1)
					.style('fill', tempColor)
			});
	
	// removing bars
	bars1
		.enter()
		.append("rect")
		.merge(bars1)
		.attr("x", 0 )
		.attr("width", function(d) { return 0; });
			
	// animation (readd bars)
	bars1
		.enter()
		.append("rect")
		.merge(bars1)
		.transition()
		.duration(500)
		.attr("x", 0 )
		.attr("width", function(d) { return x(d.prop); })
		.delay(function(d,i){return(i*50)});		

	// removing data labels
	$("#remove1").remove();
	$("#remove1").remove();

	
	// readding updated data lables
	var xAxis1 = svg1.append("g")
		.attr("transform", "translate(0," + height1 + ")")
		.attr("class", "xAxis")	

	var labels = xAxis1.selectAll("text")
		.data(data)
		.enter()
		.append("text")
		.attr("class", "label")
		.attr("id", "remove1")
		.attr("y", function(d,i) { return (height1/2.25)*i - (height1/1.45) ; }  )
		.attr("x", function(d) { return x(d.prop)- toString(d.prop).length*3; })
		.text(function (d) {
			return d.prop + "%";
		})	
}

// Initialize the plot with the first dataset
update1(All1);	