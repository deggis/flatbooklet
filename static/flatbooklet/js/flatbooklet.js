


function get_url_base () {
    return '/fb/'+flatbooklet.user;
}


var page_dashboard = function () {
    var html = new EJS({url: '/flatbooklet/templates/dashboard.ejs'}).render({ejsvar: flatbooklet.user});
    $('#content').html(html);
}

var page_new = function () {
    var html = new EJS({url: '/flatbooklet/templates/new.ejs'}).render();
    $('#content').html(html);
}

var t;

var page_timeline = function () {
    var html = new EJS({url: '/flatbooklet/templates/timeline.ejs'}).render();
    $('#content').html(html);
    
    
    $.get(get_url_base()+'/all', function (data) {
        var notes = eval('['+data+']')[0]; // FIXME: hack
        var source = {};
        source.timeline = {};
        var tl = source.timeline;
        tl.headline  = 'Note timeline';
        tl.type      = 'default';
        tl.text      = '<p>Default text; insert dates</p>';
        tl.startDate = '2013,01,07';

        var formatTime = function (time) {
          return time.toISOString().split('T')[0].replace(/\-/g,',');
        }


        var testHack = new Date();
        var items = [];
        for (key in notes) {
          var note = notes[key];
          try {
            var startDate = new Date(note.times[0]);
            var endDate   = new Date(startDate.getTime() + (24*60*1000));
              
            var item = {};
            item.startDate = formatTime(startDate);
            item.endDate   = formatTime(endDate);
            item.headline  = note.note.slice(0,30);
            item.text      = '<p>'+note.note+'</p>';
            item.assert =   {
                     "media":"<blockquote>Jesss</blockquote>",
                    "credit":"",
                    "caption":""
            };
            items.push(item);
          } catch(err) {
            //console.log('invalid date: '+note.times[0]+' with err: '+err);
            continue;
          }
        }

        
        tl.date = items.slice(0,100);
        console.log('Timeline with '+tl.date.length+ ' items');
        createStoryJS({
          type:	    'timeline',
          width:    '1000',
          height:   '600',
          source:   source,
          embed_id: 'timeline-container',
          debug:    true
        });
    });
}


function build_routes () {

    var routes = {};
    routes['#dashboard'] = page_dashboard;
    routes['#new']       = page_new;
    routes['#timeline']  = page_timeline;
    
    var open_page = function (hash) {
        $('.nav li.active').removeClass('active');
        var mainmenu_item = $('a[href='+hash+']').closest('.mainmenu');
        $(mainmenu_item).addClass('active');
        
        var page = routes[hash] || function () { alert('Page '+hash+' not implemented'); }
        page();
    }
    
    $('.nav a:not(.dropdown-toggle)').click(function () {
        open_page($(this).attr('href'));
    });

    open_page('#dashboard');
}


$(document).ready(function () {
    build_routes();
});
