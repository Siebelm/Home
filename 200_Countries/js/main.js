/*
*    main.js
*    Project 2 - Gapminder Clone
*/

var margin = {left:80, right:20, top:50, bottom: 100};
var height = 500 - margin.top - margin.bottom,
	width = 800 - margin.left - margin.right;
	
var g = d3.select("#chart-area")
		.append("svg")
			.attr("width", width+margin.left+margin.right)
			.attr("height", height+margin.top+margin.bottom)
		.append("g")
			.attr("transform", "translate(" + margin.left +
								", " + margin.top + ")");

var time = 0;
var interval;
var formattedData;

//Tooltip
var tip = d3.tip().attr("class", "d3-tip")
	.html(function(d){
		var text = "<strong>Country: </strong><span style='color:orange'>"
					+ d.country + "</span><br>";
		text += "<strong>Continent: </strong><span style='color:orange;text-transform:capitalize;'>"
					+ d.continent + "</span><br>";
		text += "<strong>Life Expectancy: </strong><span style='color:orange'>"
					+ d3.format("0.2f")(d.life_exp) + "</span><br>";
		text += "<strong>Income: </strong><span style='color:orange'>"
					+ d3.format("$,.0f")(d.income) + "</span><br>";
		text += "<strong>Population: </strong><span style='color:orange'>"
					+ d3.format(",.0f")(d.population) + "</span>";
			
		return text;
	})

g.call(tip);

//Scales

//x scale
var xScale = d3.scaleLog()
		.base(10)
		.domain([140, 150000])
		.range([0, width]);

//y scale
var yScale = d3.scaleLinear()
		.domain([0, 90])
		.range([height, 0]);

//area scale
var area = d3.scaleLinear()
		.domain([2000, 1400000000])
		.range([25*Math.PI, 1500*Math.PI]);

//continent colors
var continentColor = d3.scaleOrdinal(d3.schemeSet3);

//Labels
var xLabel = g.append("text")
		.attr("x", width / 2)
		.attr("y", height + 50)
		.attr("font-size", "20px")
		.attr("text-anchor", "middle")
		.text("Income ($)");

var yLabel = g.append("text")
		.attr("x", -170)
		.attr("y", -40)
		.attr("transform", "rotate(-90)")
		.attr("font-size", "20px")
		.attr("text-anchor", "middle")
		.text("Life Expectancy (Years)");

var timeLabel = g.append("text")
		.attr("x", width - 40)
		.attr("y", height - 10)
		.attr("font-size", "40px")
		.attr("opacity", "0.4")
		.attr("text-anchor", "middle")
		.text("1800");

//x axis
var xAxisCall = d3.axisBottom(xScale)
		.tickValues([400, 4000, 40000])
		.tickFormat(d3.format("$"));
g.append("g")
	.attr("transform", "translate(0," + height + ")")
	.call(xAxisCall);

//y axis
var yAxisCall = d3.axisLeft(yScale)
		.tickFormat(function(d){ return +d; })		
g.append("g")
	.call(yAxisCall);
	

//legend section
var continents = ["europe", "asia", "americas", "africa"];

var legend = g.append("g")
		.attr("transform", "translate(" + (width-10) + ", " +
										  (height-125) + ")");

continents.forEach(function(continent, i){
	var legendRow = legend.append("g")
			.attr("transform", "translate(0," + (i*20) + ")");
			
	legendRow.append("rect")
		.attr("width", "10")
		.attr("height", "10")
		.attr("fill", continentColor(continent));
		
	legendRow.append("text")
		.attr("x", -10)
		.attr("y", 10)
		.attr("text-anchor", "end")
		.style("text-transform", "capitalize")
		.text(continent);
}); //forEach
										  
//getting the data
d3.json("data/data.json").then(function(data){
	//console.log(data);
	
	//clean data
	formattedData = data.map(function(year){
		return year["countries"].filter(function(country){
			var dataExists = (country.income && country.life_exp)
			return dataExists;
		}).map(function(country){
			country.income = +country.income;
			country.life_exp = +country.life_exp;
			return country;
		})
	});
	
	//console.log(formattedData);
	
	//run the code every 0.1 second (100 ms) 
	//commented out the old code
	/*d3.interval(function(){
		time = (time < 214) ? time+1 : 0;
		update(formattedData[time]); //run update for current year...
	}, 100) //...every 100 ms */
	
	//first run of the visualization
	update(formattedData[0]);
	
}); //d3.json

function step() {
	time = (time < 214) ? time+1 : 0;
	update(formattedData[time]);
} //step

$("#play-button")
	.on("click", function(){
		var button = $(this);
		if (button.text() == "Play") {
			interval = setInterval(step, 100);
			button.text("Pause");
		}
		else {
			button.text("Play");
			clearInterval(interval); //Pause
		}
	})

$("#reset-button")
	.on("click", function(){
		time = 0;
		update(formattedData[time]);
	})
	
$("#continent-select")
	.on("change", function(){
		update(formattedData[time]); //make sure filter works
	})								 //during Pause

$("#date-slider").slider({
	max: 2014,
	min: 1800,
	step: 1,
	slide: function(event, ui){
		time = ui.value - 1800;
		update(formattedData[time]);
	}
})

function update(data){
	//standard transition time for the visualization
	var t = d3.transition().duration(100);
	
	var continent = $("#continent-select").val();
	
	var data = data.filter(function(d){
		if (continent == "all") {return true;}
		else {
			return d.continent == continent;
		}
	});
	
	//join new data with old elements
	var circles = g.selectAll("circle")
			.data(data, function(d){
				return d.country;
			});
			
	//exit old elements not present in new data
	circles.exit()
		.attr("class", "exit")
		.remove();
		
	//enter new elements present in our data
	circles.enter()
		.append("circle")
			.attr("class", "enter")
			.attr("fill", function(d){ 
				return continentColor(d.continent);
			})
			.on("mouseover", tip.show)
			.on("mouseout", tip.hide)
			.merge(circles)
			.transition(t)
				.attr("cy", function(d){ return yScale(d.life_exp); })
				.attr("cx", function(d) { return xScale(d.income); })
				.attr("r", function(d) { 
					return Math.sqrt(area(d.population) / Math.PI) 
				})
	
	//update the time label
	timeLabel.text(+(time + 1800));
	$("#year")[0].innerHTML = +(time+1800);
	$("#date-slider").slider("value", +(time+1800)); //make the
												//slider move as the
												//graph updates
	
} //update