Map {
  background-color: #b8dee6;
}

#countries {
  ::outline {
    line-color: #85c5d3;
    line-width: 4;
    line-join: round;
  }
  polygon-fill: #fff;
}

#contours[class=3][zoom>=10][elevation_ft > 0] {
  line-color: @contours_low;
  text-name:'[elevation_ft]';
  text-face-name:@sans;
  text-placement:line;
  text-fill:@country_text;
  text-halo-fill: @country_halo;
  [zoom=10] {
    line-width: 0.2;
  }
  [zoom>10][zoom<=12] {
   line-width: 0.4;
  }
  [zoom>=13] {

    [zoom=13]{
      line-width: 0.4;
      text-size: 9;
    }
    [zoom>=14]{
      line-width: 0.5;
    }
  }  
}
#contours[class=2][zoom>10] {
  line-color: @contours_low;
  text-name:'[elevation_ft]';
  text-face-name:@sans;
  text-placement:line;
  text-fill:@country_text;
  text-halo-fill: @country_halo;
  line-color: @contours_med;
  text-size: 0;
  [zoom>10][zoom<=12] {
   line-width: 0.2; 
  }
  [zoom=13] {
  	line-width: 0.4;
  }
  [zoom>=14]{
  	line-width: 0.5;
    text-size: 8;
  }
}

#contours[class=1][zoom>=14]{
  line-color: @contours_high;
  line-width: 0.2;
}
#hillshade {
  raster-scaling: bilinear;
  raster-comp-op: multiply;
  raster-opacity:0.2;
}
