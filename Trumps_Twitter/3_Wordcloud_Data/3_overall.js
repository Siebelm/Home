
// loading data
var All3 = [{"word":"Fake","size":"1"},{"word":"Crooked","size":"0.736462093862816"},{"word":"Bad","size":"0.595667870036101"},{"word":"Win","size":"0.51985559566787"},{"word":"Hard","size":"0.386281588447653"},{"word":"Honor","size":"0.357400722021661"},{"word":"Crime","size":"0.324909747292419"},{"word":"Strong","size":"0.306859205776173"},{"word":"Love","size":"0.191335740072202"},{"word":"Wonderful","size":"0.155234657039711"},{"word":"Congratulations","size":"0.148014440433213"},{"word":"Witch","size":"0.144404332129964"},{"word":"Enjoy","size":"0.137184115523466"},{"word":"Endorsement","size":"0.126353790613718"},{"word":"Wow","size":"0.101083032490975"},{"word":"Illegal","size":"0.0974729241877256"},{"word":"Happy","size":"0.0685920577617329"},{"word":"Collusion","size":"0.0505415162454874"},{"word":"Amazing","size":"0.0072202166064982"},{"word":"Safe","size":"0"}];


// setting the dimensions and margins of the chart
var margin = {top: 20, right: 0, bottom: 0, left: 0},
    width3 = 580 - margin.left - margin.right,
    height3 = 550 - margin.top - margin.bottom;

// append the svg object to the body of the page
var svg3 = d3.select("#_3_overall").append("svg")
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