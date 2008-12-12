//
//  lucilleAppDelegate.m
//  lucille
//
//  Created by syoyo on 11/28/08.
//  Copyright __MyCompanyName__ 2008. All rights reserved.
//

#import "lucilleAppDelegate.h"
#import "EAGLView.h"

@implementation lucilleAppDelegate

@synthesize window;
@synthesize glView;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
    
	// TODO: Write init code here.
	
	glView.animationInterval = 1.0 / 60.0;
	[glView startAnimation];
}


- (void)applicationWillResignActive:(UIApplication *)application {
	glView.animationInterval = 1.0 / 5.0;
}


- (void)applicationDidBecomeActive:(UIApplication *)application {
	glView.animationInterval = 1.0 / 60.0;
}


- (void)dealloc {
	[window release];
	[glView release];
	[super dealloc];
}

@end
