var express = require('express');
var router = express.Router();
var md = require("markdown").markdown;
var fs = require("fs");


var files = fs.readdirSync('./posts')

function parseFile (name) {
  var filePath = './posts/' + name
  var file = fs.readFileSync(filePath, "utf-8").split('\n')
  return {
    date: name.slice(0,10),
    title: file[1].slice(8, -1),
    tags: file[2].slice(6).split(' ').map(tag => {
      return {
        name: tag,
        link: "/filter/" + tag
      }
    }),
    body: md.toHTML(file.slice(5).join('\n')),
    link: filePath.slice(1, -2) + 'html'
  }
}

var posts = files.map(name => parseFile(name))


/* GET home page. */
router.get('/', function(req, res, next) {
  res.render('index', {posts: posts})
});

router.get('/posts/:name', function(req, res, next) {
  var filename = req.params.name;
  var postA = posts.filter(file => {return file.link == "/posts/" + filename})
  res.render('post', {post: postA[0]})
})

router.get('/filter/:name', function(req, res, next) {
  var tagname = req.params.name;
  var postA = posts.filter(file => {
    var checkResults = file.tags.filter(tag =>
      { return tag.link == '/filter/' + tagname})
    return checkResults.length > 0
  })
  res.render('filtered_posts', {posts: postA})
})

module.exports = router;
