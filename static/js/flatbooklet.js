


function get_url_base () {
    return '/fb/'+flatbooklet.user;
}


var page_dashboard = function () {
    var html = new EJS({url: '/templates/dashboard.ejs'}).render({ejsvar: flatbooklet.user});
    $('#content').html(html);
}

var page_new = function () {
    var html = new EJS({url: '/templates/new.ejs'}).render();
    $('#content').html(html);
}


function build_routes () {

    var routes = {};
    routes['#dashboard'] = page_dashboard;
    routes['#new']       = page_new;
    
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
