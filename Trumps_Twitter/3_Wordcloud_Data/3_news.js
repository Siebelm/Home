
// loading data
var All3 = [{"word":"Fake","size":"1"},{"word":"Enjoy","size":"0.327402135231317"},{"word":"Dishonest","size":"0.277580071174377"},{"word":"Failing","size":"0.259786476868327"},{"word":"Bad","size":"0.185053380782918"},{"word":"Phony","size":"0.0747330960854092"},{"word":"Wrong","size":"0.0711743772241993"},{"word":"Wow","size":"0.0604982206405694"},{"word":"Biased","size":"0.0498220640569395"},{"word":"Crooked","size":"0.0498220640569395"},{"word":"Witch","size":"0.0462633451957295"},{"word":"Crazy","size":"0.0355871886120996"},{"word":"Hard","size":"0.0355871886120996"},{"word":"Collusion","size":"0.0284697508896797"},{"word":"Hit","size":"0.0142348754448399"},{"word":"Win","size":"0.0142348754448399"},{"word":"Negative","size":"0.0106761565836299"},{"word":"Illegal","size":"0.00355871886120996"},{"word":"False","size":"0"},{"word":"Worse","size":"0"}];

var Trusted3 = [{"word":"Enjoy","size":"1"},{"word":"Fake","size":"0.195121951219512"},{"word":"Collusion","size":"0.0975609756097561"},{"word":"Nice","size":"0.0731707317073171"},{"word":"Illegal","size":"0.0487804878048781"},{"word":"Wow","size":"0.0487804878048781"},{"word":"Negative","size":"0.0365853658536585"},{"word":"Witch","size":"0.0365853658536585"},{"word":"Hard","size":"0.0121951219512195"},{"word":"Hit","size":"0.0121951219512195"},{"word":"Intelligence","size":"0.0121951219512195"},{"word":"Phony","size":"0.0121951219512195"},{"word":"Win","size":"0.0121951219512195"},{"word":"Bad","size":"0"},{"word":"Biased","size":"0"},{"word":"Congratulations","size":"0"},{"word":"Corrupt","size":"0"},{"word":"Crooked","size":"0"},{"word":"Won","size":"0"},{"word":"Wonderful","size":"0"}];

var Fake3 = [{"word":"Fake","size":"1"},{"word":"Dishonest","size":"0.290322580645161"},{"word":"Failing","size":"0.272401433691756"},{"word":"Bad","size":"0.186379928315412"},{"word":"Wrong","size":"0.0716845878136201"},{"word":"Phony","size":"0.0681003584229391"},{"word":"Biased","size":"0.0609318996415771"},{"word":"Wow","size":"0.0573476702508961"},{"word":"Crooked","size":"0.043010752688172"},{"word":"Enjoy","size":"0.043010752688172"},{"word":"Crazy","size":"0.03584229390681"},{"word":"Witch","size":"0.03584229390681"},{"word":"Hard","size":"0.021505376344086"},{"word":"Negative","size":"0.017921146953405"},{"word":"False","size":"0.00716845878136201"},{"word":"Hit","size":"0.00716845878136201"},{"word":"Win","size":"0.003584229390681"},{"word":"Enemy","size":"0"},{"word":"Sad","size":"0"},{"word":"Worse","size":"0"}];



// setting the dimensions and margins of the chart
var margin = {top: 20, right: 0, bottom: 0, left: 0},
    width3 = 580 - margin.left - margin.right,
    height3 = 550 - margin.top - margin.bottom;

// append the svg object to the body of the page
var svg3 = d3.select("#_3_news").append("svg")
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