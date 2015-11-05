/* ==================================================================
   ROAD & RAIL LINES
/* ================================================================== */

/* At lower zoomlevels, just show major automobile routes: motorways
and trunks. */

#roads_low[zoom>=5][zoom<=8] {
  [type='motorway'] { line-color: @motorway_line; }
  [type='trunk'] { line-color: @trunk_line; }
  [zoom=5] {
    [type='motorway'] { line-width: 0.4; }
    [type='trunk'] { line-width: 0.2; } }
  [zoom=6] {
    [type='motorway'] { line-width: 0.5; }
    [type='trunk'] { line-width: 0.25; } }
  [zoom=7] {
    [type='motorway'] { line-width: 0.6; }
    [type='trunk'] { line-width: 0.3; } }
  [zoom=8] {
    [type='motorway'] { line-width: 1; }
    [type='trunk'] { line-width: 0.5; } }
}

/* At mid-level scales start to show primary and secondary routes
as well. */

#roads_med[zoom>=9][zoom<=10] {
  [type='motorway'],
  [type='motorway_link'] {
    line-color: @motorway_line;
  }
  [type='trunk'],
  [type='trunk_link'] {
    line-color: @trunk_line;
  }
  [type='primary'] { line-color: @primary_line; }
  [type='secondary'] { line-color: @secondary_line; }
  [type='tertiary'] { line-color: @standard_line; }
  [zoom=9] {
    [type='motorway'],[type='trunk'] { line-width: 1.4; }
    [type='primary'],[type='secondary'],
    [type='motorway_link'],[type='trunk_link'] { line-width: 0.6; }
  }
  [zoom=10] {
    [type='motorway'],[type='trunk'] { line-width: 1.8; }
    [type='primary'],[type='secondary'],
    [type='motorway_link'],[type='trunk_link'] { line-width: 0.8; }
  }
}

/* At higher levels the roads become more complex. We're now showing 
more than just automobile routes - railways, footways, and cycleways
come in as well.

/* Road width variables that are used in road & bridge styles */
@rdz11_maj: 1.6; @rdz11_med: 0.8; @rdz11_min: 0.4;
@rdz12_maj: 2.5; @rdz12_med: 1.2; @rdz12_min: 0.8;
@rdz13_maj: 3;   @rdz13_med: 1.5; @rdz13_min: 1;
@rdz14_maj: 4;   @rdz14_med: 2.5; @rdz14_min: 1.6;
@rdz15_maj: 6;   @rdz15_med: 4;   @rdz15_min: 2;
@rdz16_maj: 8;   @rdz16_med: 6;   @rdz16_min: 4;
@rdz17_maj: 14;  @rdz17_med: 12;  @rdz17_min: 10;
@rdz18_maj: 20;  @rdz18_med: 17;  @rdz18_min: 14;

/* ---- Casing ----------------------------------------------- */

#roads_high::outline[zoom>=11][zoom<=20]{
  /* -- colors & styles -- */
  line-cap: round;
  [bridge=1],
  [tunnel=1] {
    line-cap: butt;
  }
  line-join: round;
  line-color: @standard_case;
  [bridge=1] { line-color: @standard_case * 0.8; }
  [type='motorway'],
  [type='motorway_link'] {
    line-color: @motorway_case;
    [bridge=1] { line-color: @motorway_case * 0.8; }
  }
  [type='trunk'],
  [type='trunk_link'] {
    line-color: @trunk_case;
    [bridge=1] { line-color: @trunk_case * 0.8; }
  }
  [type='primary'],
  [type='primary_link'] {
    line-color: @primary_case;
    [bridge=1] { line-color: @primary_case * 0.8; }
  }
  [type='secondary'],
  [type='secondary_link'] {
    line-color: @secondary_case;
    [bridge=1] { line-color: @secondary_case * 0.8; }
  }
  [class='railways'] {
    line-color: fadeout(@land,50%);
    [bridge=1] { line-color: @secondary_case * 0.8; }
  }
  [tunnel=1] { line-dasharray: 3,3; }        
  /* -- widths -- */
  [zoom=11] {
    [class='motorways'] { line-width: @rdz11_maj + 2; }
    [class='mainroads'] { line-width: @rdz11_med + 1.6; }
    [class='minorroads']{ line-width: @rdz11_min; }
    /* No minor bridges yet */
    [class='railways']  { line-width: 0; }
  }
  [zoom=12] {
    [class='motorways'] { line-width: @rdz12_maj + 2; }
    [class='mainroads'] { line-width: @rdz12_med + 2; }
    [class='minorroads']{ line-width: @rdz12_min; }
    /* No minor bridges yet */
    [class='railways']  { line-width: 0; }
  }
  [zoom=13] {
    [class='motorways'] { line-width: @rdz13_maj + 2; }
    [class='mainroads'] { line-width: @rdz13_med + 2; }
    [class='minorroad']{ line-width: @rdz13_min + 2; }
    [class='railways']  { line-width: 0; }
  }
  [zoom=14] {
    [class='motorways'] { line-width: @rdz14_maj + 2; }
    [class='mainroads'] { line-width: @rdz14_med + 2; }
    [class='minorroads']{ line-width: @rdz14_min + 2; }
    [class='railways']  { line-width: 0; }
  }
  [zoom=15] {
    [class='motorways'] { line-width: @rdz15_maj + 2.5; }
    [class='mainroads'] { line-width: @rdz15_med + 2; }
    [class='minorroads']{ line-width: @rdz15_min + 2; }
    [class='railways']  { line-width: 1.5 + 2; }
  }
  [zoom=16] {
    [class='motorways'] { line-width: @rdz16_maj + 2.5; }
    [class='mainroads'] { line-width: @rdz16_med + 2.5; }
    [class='minorroads']{ line-width: @rdz16_min + 2; }
    [class='railways']  { line-width: 2 + 2; }
  }
  [zoom>=17] {
    [class='motorways'] { line-width: @rdz17_maj + 3; }
    [class='mainroads'] { line-width: @rdz17_med + 2.5; }
    [class='minorroads']{ line-width: @rdz17_min + 2; }
    [class='railways']  { line-width: 3 + 4; } // 3 + 4
  }
  [zoom>=18] {
    [class='motorways'] { line-width: @rdz18_maj + 4; }
    [class='mainroads'] { line-width: @rdz18_med + 4; }
    [class='minorroads']{ line-width: @rdz18_min + 3.5; }
    [class='railways']  { line-width: 4 + 6; }
  }
}


#roads_high[zoom>=11][zoom<=20]{
  /* -- colors & styles -- */
  line-color: @standard_fill;
  [type='motorway'],
  [type='motorway_link'] {
    line-color: @motorway_fill;
    [tunnel=1] { line-color: lighten(@motorway_fill, 10%); }
  }
  [type='trunk'],
  [type='trunk_link'] {
    line-color: @trunk_fill;
    [tunnel=1] { line-color: lighten(@trunk_fill, 10%); }
  }
  [type='primary'],
  [type='primary_link'] {
    line-color: @primary_fill;
    [tunnel=1] { line-color: lighten(@primary_fill, 10%); }
  }
  [type='secondary'],
  [type='secondary_link'] {
    line-color: @secondary_fill;
    [tunnel=1] { line-color: lighten(@secondary_fill, 10%); }
  }
  [class='railways'] {
    line-color: @rail_line;
    line-dasharray: 1,1;
    [type='subway'] { line-opacity: 0.67; }
    [zoom>15] { line-dasharray: 1,2; } 
  }
  [class='class'] {
    line-width: 0;
  }
  [class='minorroad'],
  [class='mainroad'],
  [class='motorway'] {
    line-cap: round;
    line-join: round;
  }
  [tunnel=1] {
    line-cap: butt;
  }
  /* -- widths -- */
  [zoom=11] {
    [class='motorways'] { line-width: @rdz11_maj; }
    [class='mainroads'] { line-width: @rdz11_med; }
    [class='minorroads']{ line-width: 0; }
    [class='railways']  { line-width: 0.2; }
  }
  [zoom=12] {
    [class='motorways'] { line-width: @rdz12_maj; }
    [class='mainroads'] { line-width: @rdz12_med; }
    [class='minorroads']{ line-width: 0; }
    [class='railways']  { line-width: 0.4; }
  }
  [zoom=13] {
    [class='motorways'] { line-width: @rdz13_maj; }
    [class='mainroads'] { line-width: @rdz13_med; }
    [class='minorroads']{ line-width: @rdz13_min; }
    [class='railways']  { line-width: 0.8; }
  }
  [zoom=14] {
    [class='motorways'] { line-width: @rdz14_maj; }
    [class='mainroads'] { line-width: @rdz14_med; }
    [class='minorroads']{ line-width: @rdz14_min; }
    [class='railways']  { line-width: 1; }
  }
  [zoom=15] {
    [class='motorways'] { line-width: @rdz15_maj; }
    [class='mainroads'] { line-width: @rdz15_med; }
    [class='minorroads']{ line-width: @rdz15_min; }
    [class='railways']  { line-width: 1.5; }
  }
  [zoom=16] {
    [class='motorways'] { line-width: @rdz16_maj; }
    [class='mainroads'] { line-width: @rdz16_med; }
    [class='minorroads']{ line-width: @rdz16_min; }
    [class='railways']  { line-width: 2; }
  }
  [zoom=17] {
    [class='motorways'] { line-width: @rdz17_maj; }
    [class='mainroads'] { line-width: @rdz17_med; }
    [class='minorroads']{ line-width: @rdz17_min; }
    [class='railways']  { line-width: 3; }
  }
  [zoom>=18] {
    [class='motorways'] { line-width: @rdz18_maj; }
    [class='mainroads'] { line-width: @rdz18_med; }
    [class='minorroads']{ line-width: @rdz18_min; }
    [class='railways']  { line-width: 4; }
  }
}


/******************************************************************* */