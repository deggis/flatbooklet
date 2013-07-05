<apply template="base">

  <ifLoggedIn>
    <apply template="_app"/>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>

</apply>
