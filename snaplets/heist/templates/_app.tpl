
<div class="navbar navbar-fixed-top">
  <div class="navbar-inner">
    <div class="container">
      <a class="brand" href="#">Flatbooklet</a>
        <ul class="nav">
          <li class="active">
            <a href="#dashboard">
              <i class="icon-home icon-white"></i>Dashboard
            </a>
          </li>
          <li>
            <a href="#new">New</a>
          </li>
          <li class="dropdown">
            <a class="dropdown-toggle" data-toggle="dropdown" id="dLabel" href="#all">
              View 
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu">
              <li>
                <a href="#view-last20">Last 20</a>
              </li>
              <li>
                <a href="#view-last100">Last 100</a>
              </li>
              <li class="divider"></li>             
              <li>
                <a href="#view-all">All</a>
              </li>
              <li>
                <a href="#view-tags">Tags</a>
              </li>
              <li>
                <a href="#view-timeline">Timeline</a>
              </li>
              <li class="divider"></li>             
              <li>
                <a href="#statistics">Statistics</a>
              </li>
           </ul>
          </li>
          <li class="divider-vertical"></li>
          <li>
            <a href="/logout">Logout</a>
          </li>
        </ul>
        <form class="navbar-search pull-right" action="">
          <input type="text" class="search-query span3" placeholder="Search">
        </form>
    </div>
  </div>
</div>

<div id="content" class="container">


  <!-- dummy elements to test JS functionality -->
  <div id="topic">
    <h2>Flatbooklet</h2>
  </div>
  <div id="statistics-container">
    <h3>Statistics</h3>
    <div id="statistics">
    </div>
  </div>
  <div id="viewer">
    <h3>View/update</h3>
    <fieldset>
      <legend>Viewer</legend>
      <label for="viewer-textarea">(add new)</label>
      <textarea rows="6" cols="60" id="viewer-textarea"></textarea>
      <button id="viewer-send" disabled>Add</button>
    </fieldset>
  </div>
  <div id="search">
    <h3>Search</h3>
    <fieldset>
      <legend>Search</legend>
      <input type="text" size="30" />
      <button id="search-button">Search</button>
    </fieldset>
  </div>
  <div id="latest-container">
    <h3>Latest</h3>
    <div id="latest">
    </div>
  </div>
  <div id="timeline-container">
    <h3>Timeline</h3>
    <div id="timeline">
    </div>
  </div>
  
  <div id="footer">
  </div>
</div>