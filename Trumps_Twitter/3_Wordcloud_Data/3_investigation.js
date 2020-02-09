
// loading data
var All3 = [{"word":"Crime","size":"1"},{"word":"Witch","size":"0.707006369426752"},{"word":"Collusion","size":"0.630573248407643"},{"word":"Fake","size":"0.496815286624204"},{"word":"Crooked","size":"0.439490445859873"},{"word":"Strong","size":"0.248407643312102"},{"word":"Supreme","size":"0.178343949044586"},{"word":"Bad","size":"0.171974522292994"},{"word":"Illegal","size":"0.152866242038217"},{"word":"Endorsement","size":"0.140127388535032"},{"word":"Phony","size":"0.121019108280255"},{"word":"Loves","size":"0.10828025477707"},{"word":"Hoax","size":"0.089171974522293"},{"word":"Angry","size":"0.0828025477707006"},{"word":"Criminal","size":"0.0700636942675159"},{"word":"Win","size":"0.0445859872611465"},{"word":"Obstruction","size":"0.0382165605095541"},{"word":"Weak","size":"0.0127388535031847"},{"word":"Hard","size":"0"},{"word":"Tough","size":"0"}]
;


// setting the dimensions and margins of the chart
var margin = {top: 20, right: 0, bottom: 0, left: 0},
    width3 = 580 - margin.left - margin.right,
    height3 = 550 - margin.top - margin.bottom;

// append the svg object to the body of the page
var svg3 = d3.select("#_3_investigation").append("svg")
    .attr("width", width3 + margin.left + margin.right)
    .attr("height", height3 + margin.top + margin.bottom)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

// adding title
var Title = svg3.append("text")
		.attr("y", -5)
		.attr("font-size", "1.75em")
		.attr("text-anchor", "right")
		.text("Top Words Used");	


// updating data
function update3(data) {		

// creating word cloud
var layout = d3.layout.cloud()
  .size([width3, height3])
  .words(data.map(function(d) { return {text: d.word, size:d.size}; }))
  .padding(5)        //space between words
  .rotate(function() { var angle = ~~(Math.random() * 4)  * 45
							if (angle==135) {
								var flip = -45; 
								return flip;
							}
							else {
								return angle; 	
							}
						})
  .fontSize(function(d) { return d.size*90+30; })
  .on("end", draw);
layout.start();

// drawing top 20 words
function draw(words) {
	$("#remove3").remove();
	$("#remove3").remove();
	$("#remove3").remove();
	$("#remove3").remove();

	svg3
    .append("g")
	.attr("id", "remove3")
		.attr("transform", "translate(" + layout.size()[0] / 2 + "," + layout.size()[1] / 2 + ")")
		.selectAll("text")
			.data(words)
		.enter().append("text")
		.style("font-size", function(d) { return d.size; })
		.style("fill", "#A57706")
		.attr("text-anchor", "middle")
		.style("font-family", "Impact")
		.attr("transform", function(d) {
		return "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")";
		})
		.text(function(d) { return d.text; })
		.on("mouseover", function(d){
			tempColor = this.style.fill;
			d3.select(this)
				.transition() 
				.style('fill', 'rgb(220, 217, 205)')
				.transition().delay(750).duration(5000)
				.style('fill', tempColor);;
			})
		.on("mouseout", function(d){
				d3.select(this)
					.transition().duration(500) 
					.style('fill', tempColor)
			})		
	}
}
// Initialize the plot with the first dataset
update3(All3);	