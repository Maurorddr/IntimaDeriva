SuenaMiEskeleto{

	var <>eskeleto, <nUsers, <che, <parser, chePlayerD, cheConductor, chePlayerI;
	var <>pizBuf1, <>chaBuf2, <>suckMyBuf, <>jarBuf1, <>benBuf1;
	var <>bufing1, <>bufing2, <>bufing3, <>bufing4, <>bufCon1, <>bufCon2;
	var <>in1Buf, <>cello1Buf, <>cello2Buf, <>cello3Buf, <>chelaTesta, <>agua;
	var <>cheConductorI, <>cheConductorD, densidad, densidadBuf;

	var r1, r2, r3, r4, r5, r6, seq1, seq2, seq3, seq4, pb1, pb2, pb3, pb4, pb5, pb6, pitch;
	var <>gui, w, <>manoIButton, <>manoIbVal, <>manoDButton, <>manoDbVal;
	var <>pieIButton, <>pieDButton, <>pieIVal, <>pieDVal, <>pieIFmButton, <>pieIFmVal, <>pieDFmButton, <>pieDFmVal;
	var <>rodillaIButton, <>rodillaIbVal, <>rodillaDButton, <>rodillaDbVal, <>chamorroIButton, <>chamorroIbVal;
	var <>codoIButton, <>codoIbVal, <>codoDButton, <>codoDbVal, <>chamorroDButton, <>chamorroDbVal;
	var <>manoITorsoButton, <>manoITbVal, <>manoDTorsoButton, <>manoDTbVal, <>ochobitButton, <>ochobitVal;
	var <>cabezaCaderaButton, <>balanceVal;
	var <>playButton, cmdPeriodFunc, tick, <>chamorroIButton2, inMin, inMax, outMin, outMax;
	var <>laberintoButton, <>laberinto, <escena;
	var cinderAddr, cinderAddr2;

	//--------------------------------
	//--------    new   --------------
	//--------------------------------
	*new{
		^super.new.init();
	}

	//--------------------------------
	//---------   init   -------------
	//--------------------------------
	init{

		eskeleto = ReceiveSkeletons.new;
		tick = 0;
		escena = 0;
		gui = GUI.current;
		suckMyBuf = nil;
		cheConductorI = nil;
		cheConductorD = nil;
		pitch = 12;
		cinderAddr = NetAddr("169.254.229.56", 3000);
		cinderAddr2 = NetAddr("192.168.100.8", 60000);
		inMin = [0.25]; inMax = [0.65]; outMin = [0.25]; outMax = [0.65];

		//--   Buffers  ---
		in1Buf = Buffer.alloc(Server.default, 4*Server.default.sampleRate, 2, bufnum: 10);
		in1Buf.zero;

		cello1Buf = Buffer.read(Server.default, Platform.resourceDir +/+ "sounds/rockdrigo.wav", bufnum: 11);

		cello2Buf = Buffer.read(Server.default, Platform.resourceDir +/+ "sounds/cello8.wav", bufnum: 12);

		cello3Buf = Buffer.read(Server.default, Platform.resourceDir +/+ "sounds/cello3.wav", bufnum: 13);

		pizBuf1 = Buffer.readChannel(Server.default, Platform.resourceDir +/+ "sounds/cello3M.wav", 0, bufnum: 14);
		chaBuf2 = Buffer.readChannel(Server.default, Platform.resourceDir +/+ "sounds/tibetanChant3.wav", channels: 0, bufnum: 15);
		jarBuf1 = Buffer.readChannel(Server.default, Platform.resourceDir +/+ "sounds/jarana_01.wav", 0, bufnum: 16);
		benBuf1 = Buffer.readChannel(Server.default, Platform.resourceDir +/+ "sounds/mambomex1L.wav", 0, bufnum: 17);
		bufing1 = Buffer.read(Server.default, Platform.resourceDir +/+ "sounds/TrenLento.wav", bufnum:18);
		bufing2 = Buffer.read(Server.default, Platform.resourceDir +/+ "sounds/cello2.wav", bufnum:19);
		bufing3 = Buffer.read(Server.default, Platform.resourceDir +/+ "sounds/firecrackling.aif", bufnum:20);
		bufing4 = Buffer.read(Server.default, Platform.resourceDir +/+ "sounds/vaca.wav", bufnum:21);

		bufCon1 = Buffer.readChannel(Server.default, Platform.resourceDir +/+ "sounds/birds.wav", channels: 0, bufnum:22);
		bufCon2 = Buffer.readChannel(Server.default, Platform.resourceDir +/+ "sounds/cuerda.wav", channels: 0, bufnum:23);
		agua = Buffer.readChannel(Server.default, Platform.resourceDir +/+ "sounds/aguahirviendo.wav", channels: 0, bufnum:24);



		//--   Sintes  ------------------

		//-------------------------------
		//--   Fiero  -------------------
		SynthDef(\fiero, {|gate = 1, note = 40, dur = 1, bufn = 24, arate = 0.5|
			var env, freq, comb, sig, chain, chain1, chain2, sin, arr, n, conv, kernel, trig;
			n = 7;
			note = note;// + (25 - MouseX.kr(-20,45).round(7));
			trig = Changed.kr(note, 1);
			//	env = EnvGen.kr(Env.perc(0.01, 0.99), gate, timeScale:dur, doneAction:2);
			env = EnvGen.kr(Env.asr(5, 1,5), gate, doneAction:2);
			freq = note.midicps;
			arr = Array.fill(n, {|i| i*freq*LFSaw.kr(TRand.kr(0.1, 0.3, trig),TRand.kr(0, 2pi, trig)).range(0.69, 0.9)});
			sin = Mix(Splay.ar(SinOsc.ar(arr+SinOsc.ar(arr, 0, 100), TRand.kr(-pi, pi, trig) , 0.1)* Lag.ar(Pulse.ar( Array.fill(n, {LFSaw.kr(rrand(0.3,0.5), rrand(-pi,pi)).range(0.5,5)}), Array.fill(n, {LFSaw.kr(rrand(0.3,0.5), rrand(-pi,pi)).range(0.125,0.75)}) ),0.05)));

			//kernel= PlayBuf.ar(1,~agua, (MouseX.kr(-6,12).midiratio),1,0,1);
			kernel= PlayBuf.ar(1, bufn, arate, 1, 0, 1);
			conv = Limiter.ar(Convolution.ar(sin, kernel, 2*2048, 0.125), 0.8);
			//comb = conv;
			//comb =Decay.ar(conv, 0.01, 0.005);
			//comb =(CombN.ar(Decay.ar(conv, 0.01, 0.005), 5, MouseY.kr(0.05, 5), 5));
			//comb =(CombN.ar(sin, 5, scrach, 5));

			Out.ar(0, Pan2.ar(conv*env, LFNoise1.ar(0.3)));
		}).add;

		//-------------------------------
		//--   Sinte fmRes  ---

		SynthDef(\fmRes, {|amp = 0.5, gate = 1, pos = 0, pos1 = 0, note = 40, dur = 2.1, indx1 = 5, indx2 = 5, rate = 3, lfo1 = 40|

			var env, env1, port1,port2, mod1, out, lfo2, freq, klng, dust, snd;



			var scale1 = [0, 0, 0, 0, 0, 0, 0];
			var scale2 = [11, 4, 2, 4, 2, 4, 11];
			var scale3 = [7, 7, 7, 8, 7, 7, 7];

			//var scale = [0, -2, 4, -7, 9, -2, 5, 7, -6, 6 , -6, 7, 5, -2, 9, -7, 4, -2, 0];
			var fmBuf1 = LocalBuf(scale1.size, 1).set(scale1);
			var fmBuf2 = LocalBuf(scale2.size, 1).set(scale2);
			var fmBuf3 = LocalBuf(scale3.size, 1).set(scale3);

			var degree;

			degree = [Index.kr(fmBuf1, pos1*BufFrames.ir(fmBuf1)),
				Index.kr(fmBuf2, pos1*BufFrames.ir(fmBuf2)),
				Index.kr(fmBuf3, pos1*BufFrames.ir(fmBuf3))];

			snd = degree.do({|item, indx|
				freq = (note+item).midicps;
				env = EnvGen.kr(Env.asr(8, 1, 8), gate, doneAction: 2);

				dust = Dust.ar(rate, 1);
				env1 = EnvGen.ar(Env.perc(0.2,0.8,1), dust, timeScale: 1,  doneAction: 0);
				klng =DynKlank.ar(`[[freq, 2.12*freq, 3.345*freq, 4.57*freq, freq*6.89],[0.1, 0.6, 0.56, 0.45, 0.29],[3.21, 2.78, 1.90, 0.9, 0.8]], dust)*0.5;

				/*indx1 = MouseX.kr(10, 2*freq);
				indx2 = MouseY.kr(10, 2*freq);*/

				//lfo1 = MouseX.kr(40, 400);
				lfo2 = SinOsc.ar(0.001,0,1).range(0.4975, 0.5125);

				//mod1 = SinOsc.ar((freq*MouseX.kr(0.1,2).poll), 0, indx);
				//mod1 = SinOsc.ar([(freq*1.25*lfo2), (freq*lfo2)], 0, [indx1, indx2]);
				mod1 = SinOsc.ar((freq*1.25*lfo2), 0, [indx1, indx2].choose);
				//port1 = SinOsc.ar([(freq + mod1), (0.25*freq + mod1)], 0, 0.3);
				port1 = SinOsc.ar((0.25*freq + mod1), 0, 0.3);
				//port2 = SinOsc.ar([(1.12*freq + mod1), (2.0001*freq + mod1)], 0, 0.125);
				port2 = SinOsc.ar((2.0001*freq + mod1), 0, 0.125);
				//amp = Lag.kr(amp, 0.75);
				out = LPF.ar(port1+port2+klng, lfo1)*0.5;

				//out = out + klng;
				out = Limiter.ar((out*env*amp), 0.99);
				out = Pan2.ar(out, SinOsc.kr(0.125,0,1));

				Out.ar(0, out);
			});



		}).add;


		//d = Synth(\fmRes);
		//s.meter
		//d.set(\note, 50)

		//-------------------------------
		//--   Sinte suckMyGrainBuff  ---

		SynthDef(\suckMyGrainBuff, {|out = 0, des = 0, buf1 = 14, buf2 = 15, prate = 1, trate = 1.25, gate = 1, ampUno = 0.1, pos1 = 0.5, pos2 = 0.5, dur1 = 0.25 , dur2 = 0.25|
			var player1, player2, chain, chain2, ifft, ifft2, env, trig, scale, scaleBuf, degree;

			scale = ([[-20, -16, -13], [-18, -15, -10], [-16, -13, -8], [-12, -8, -4], [-9, -5, -1], [-4, 0, 4], [0, 4, 8], [3, 7, 11], [8, 10, 11]]).midiratio;
			scaleBuf = LocalBuf(scale.size, 1).set(scale);
			degree = Index.kr(scaleBuf, prate*BufFrames.ir(scaleBuf));


			env = EnvGen.kr(Env.asr(8,1,8), gate, doneAction: 2);

			player1 = Mix(Warp1.ar(1, buf1, pos1, degree, dur1, overlaps:8, windowRandRatio: 0.03));
			player2 = Mix(Warp1.ar(1, buf2, pos2, degree, dur2, overlaps:8, windowRandRatio: 0.03));



			chain2 = FFT({LocalBuf(512, 1)}.dup, player2);
			chain = FFT({LocalBuf(512, 1)}.dup, player1);

			//ifft2 = IFFT(PV_CopyPhase(chain, chain2));
			chain2 = PV_CopyPhase(chain2, chain);
			chain2 = PV_BinShift(chain2, LFNoise1.kr(0.3).range(0.975,1.1250),0);
			chain2 = PV_MagShift(chain2, prate+0.5);

			ifft = IFFT(chain2);


			Out.ar(0, (ifft) * env * ampUno);

		}).add;

		//-------------------------------
		//--   Sinte suckMyGrainBuff2  ---

		SynthDef(\suckMyGrainBuff2, {|out = 0, des = 0, pos = 0, buf1 = 14, buf2 = 15, prate = 1, trate = 1.25, gate = 1, ampUno = 0.1, pos1 = 0.5, pos2 = 0.5, dur1 = 0.25 , dur2 = 0.25|
			var player1, player2, chain, chain2, ifft, env, trig, scale, scaleBuf, densidad;
			var arm1, arm2, arm3, arm1Buf, arm2Buf, arm3Buf, degree, armUno, armDos, armTre, densidadBuf;

			/*pos1 = MouseX.kr(0,1);
			pos2 = 1 - MouseX.kr(0,1);

			prate = MouseY.kr(0,1);*/

			//scale = [-22, -18, -14, -10, -6, -2, 3, 7, 11];
			arm1 = [-22, -18, -14, -10, -6, -2, 3, 7, 11];
			arm2 = [-22, -19, -14, -11, -6, -2, 2, 7, 12]-5;
			//arm3 = [5, 2, -2, -9, 2, 4, 8, -5, -9, 5];

			densidad = Array.fill(15, {poisson(12)});

			arm1Buf = LocalBuf(arm1.size, 1).set(arm1);
			arm2Buf = LocalBuf(arm2.size, 1).set(arm2);
			//arm3Buf = LocalBuf(arm3.size, 1).set(arm3);

			//scaleBuf = LocalBuf(scale.size, 1).set(scale);

			densidadBuf = LocalBuf(densidad.size, 1).set(densidad);

			degree = Index.kr(densidadBuf, prate*BufFrames.ir(densidadBuf));

			armUno = Index.kr(arm1Buf, prate*BufFrames.ir(arm1Buf));
			armDos = Index.kr(arm2Buf, prate*BufFrames.ir(arm2Buf));
			//armTre = Index.kr(arm3Buf, pos*BufFrames.ir(arm3Buf));

			env = EnvGen.kr(Env.asr(8,1,8), gate, doneAction: 2);

			player1 = Warp1.ar(1, buf1, pos1, (armUno+degree).midiratio, dur1, overlaps:8, windowRandRatio: 0.03);
			player2 = Warp1.ar(1, buf2, pos2, (armDos+degree).midiratio, dur2, overlaps:8, windowRandRatio: 0.03);


			chain2 = FFT({LocalBuf(512, 1)}.dup, player2);
			chain = FFT({LocalBuf(512, 1)}.dup, player1);

			chain2 = PV_CopyPhase(chain2, chain);

			ifft = IFFT(chain2);

			Out.ar(0, ifft * env * ampUno);

		}).add;

		//-------------------------------
		//--   Sinte che   --------------
		SynthDef(\che, {|gate = 1, amp = 0.5, freq = 440, dur = 1, bw = 0|

			var env, filtT, out, bNoise, ptc;


			env = EnvGen.kr(Env.linen(0.005, 0.01, 0.97, curve:-8), gate, timeScale: dur, doneAction:2);

			/*manD = [-750,750].asSpec.unmap(manoDY);
			manI = [-750,750].asSpec.unmap(manoIY);
			manDz = [1000,3000].asSpec.unmap(manoDZ);
			manIz =[1000,3000].asSpec.unmap(manoIZ);
			tor = [-1250,1250].asSpec.unmap(torsoX);*/

			bNoise = BrownNoise.ar(1);

			filtT = BPF.ar([bNoise, bNoise], ([freq, freq*3.midiratio]), [ (XLine.kr(2, 30, dur))/freq, (XLine.kr(3, 20, dur))/(freq*3)], [10, 10]);

			filtT = Limiter.ar(filtT, 0.9);
			//ptc = PitchShift.ar(filtT, 0.2, 1, A2K.kr(bw));

			Out.ar(0, filtT*amp*env);
			//Out.ar(0, ptc*amp*env);

		}).add;

		//-------------------------------
		//--   Sinte procesDance   ------

		SynthDef(\procesDance, {|buf1 = 13, buf2 = 13, buf3= 13, preLevel = 1, recLevel = 1, rate = 1, amp = 0.125, gate = 1, dur = 8, pos = 0.5, grSize = 1, ovr = 7, pan = 0, wrr = 0|

			var env,  play, in, rec, run, loop, trig, warp1, warp2, warp3, volumen, densidad;
			var arm1, arm2, arm3, arm1Buf, arm2Buf, arm3Buf, degree, armUno, armDos, armTre, densidadBuf;

			//var scale = [0, -2, -4, -7, -9, -12, -15, -17, -19,-24, -19, -17, -15, -12, -9, -7, -4, -2, 0];
			//var scale = [0, -2, 4, -7, 9, -2, 5, 7, -6, 6 , -6, 7, 5, -2, 9, -7, 4, -2, 0];
			/*arm1 = [0, 7, 9, -3, 12, 0, 4, 7, 2, 0];
			arm2 = [7, 5, 4, 0, -5, 7, 9, 0, -5, 7];
			arm3 = [5, 2, -2, -9, 2, 4, 8, -5, -9, 5];
			*/

			arm1 = [ -11, -10, -9, -7, -5, -4, -2];
			arm2 = [ -5, -4, -2, -11, -10, -9, -7];
			arm3 = [ -10, -9, -7, -5, -4, -2, -11];

			densidad = Array.fill(15, {poisson(24)-12});
			//densidad = (-12..12);

			arm1Buf = LocalBuf(arm1.size, 1).set(arm1);
			arm2Buf = LocalBuf(arm2.size, 1).set(arm2);
			arm3Buf = LocalBuf(arm3.size, 1).set(arm3);
			densidadBuf = LocalBuf(densidad.size, 1).set(densidad);

			degree = Index.kr(densidadBuf, pos*BufFrames.ir(densidadBuf));

			armUno = Index.kr(arm1Buf, pos*BufFrames.ir(arm1Buf));
			armDos = Index.kr(arm2Buf, pos*BufFrames.ir(arm2Buf));
			armTre = Index.kr(arm3Buf, pos*BufFrames.ir(arm3Buf));

			volumen = Lag.kr(amp, 0.75);

			env = EnvGen.kr(Env.asr(8, 1, 8), gate, doneAction: 2);

			warp1 = Warp1.ar(2, buf1, LFNoise2.kr(17,0.01) + pos, (armUno).midiratio, grSize, windowRandRatio: wrr, overlaps: ovr, interp: 4);
			warp2 = Warp1.ar(2, buf2, LFNoise2.kr(25,0.01) + pos, (armDos).midiratio, grSize, windowRandRatio: wrr, overlaps: ovr, interp: 4);
			warp3 = Warp1.ar(2, buf3, LFNoise2.kr(13,0.01) + pos, (armTre).midiratio, grSize, windowRandRatio: wrr, overlaps: ovr, interp: 4);
			Out.ar(0, Pan2.ar((warp1+warp2+warp3)* volumen * env, pan));

		}).add;

		//----------------------------------
		//--------- ScrachIt -----------

		SynthDef(\scrachIt, {|pos = 0.5, buf = 18, amp = 0.5, gate = 1|
			var env, reader, out, ps, sin;

			env = EnvGen.kr(Env.asr(8, 1, 8), gate, doneAction:2);

			reader = BufRd.ar(2, buf, BufFrames.ir(buf)*Lag.ar(K2A.ar(pos),0.01));
			ps = PitchShift.ar(reader, 0.2, 0.25, 1, 2);
			sin = SinOsc.ar((ps*[400,200])+(reader*[100,150]), 0, 1)*reader;

			Out.ar(0, sin*env*Lag.kr(amp, 0.75));

		}).add;

		//----------------------------------
		//--------- ScrachIt2 -----------

		SynthDef(\scrachIt2, {|pos = 0.5, buf = 19, amp = 0.5, gate = 1, height = 20, rate = 1|
			var env, reader, out, kln, sin;

			//rate = MouseY.kr(0.5, 20);
			var scale = [0, -2, 4, -7, 9, -2, 5, 7, -6, 6 , -6, 7, 5, -2, 9, -7, 4, -2, 0].midiratio;
			var fmBuf = LocalBuf(scale.size, 1).set(scale);
			var degree = Index.kr(fmBuf, pos*BufFrames.ir(fmBuf));

			env = EnvGen.kr(Env.asr(8, 1, 8), gate, doneAction:2);
			reader = BufRd.ar(2, buf, BufFrames.ir(buf)*Lag.ar(K2A.ar(pos), 0.01));

			kln = DynKlank.ar(`[Array.fill(6,{|i| ((i+1)*0.125)*rrand(80, 1000)}),
				Array.fill(6,{|i| (i * (0.5-rrand(0.00001, 0.05)))+0.00001}),
				Array.fill(6,{rrand(0.01, 0.25)})], reader, freqscale:degree, freqoffset: height);
			kln = Limiter.ar(kln, 0.9);
			sin = SinOsc.ar((reader*[150, 300])+(kln*[200, 300]), 0, 1) * kln;

			Out.ar(0, (sin+kln)*env*amp);

		}).add;

		//-------------------------------------
		//-------- ordilla --------------------

		SynthDef(\ordilla, {|gate = 1, amp = 0.5, buf1 = 23, buf2 = 22, pos = 0, ratio = -12, pDisp = 0, tDisp = 0|
			var env, in1, in2, out, conv, play, ps;
			var scale = ([0, -2, 4, -5, 9, -2, 5, 7, -6, 6 , -6, 7, 5, -2, 9, -5, 4, -2, 0]);
			var scaleBuf = LocalBuf(scale.size, 1).set(scale);
			var degree = Index.kr(scaleBuf, pos*BufFrames.ir(scaleBuf));

			env = EnvGen.kr(Env.asr(10,1,10), gate, doneAction:2);

			play = Warp1.ar(1, buf2, LFNoise1.kr(0.1).range(0,1), 1, windowSize: 0.5, overlaps: 15, windowRandRatio:0.25, mul:1.5);
			conv = Convolution2L.ar(play, buf1, framesize: 256, mul:1.0);
			ps = PitchShift.ar(conv, 0.2, (ratio+degree).midiratio, pDisp, tDisp);

			Out.ar(0, (ps!2)*env*amp);
			}

		).add;

		//-------------------------------------
		//------ campana1 ---------------------

		SynthDef(\campana1, {|pos = 0.5, buf = 18, amp = 0.5, gate = 1, rate = 0.5, ratio = 0, bw =50|
			var env, reader, out, kln, sin, trig;

			//rate = MouseX.kr(1,20);
			trig = Dust.kr(rate);
			env = EnvGen.kr(Env.asr(8, 1, 8), gate, doneAction:2);
			reader = BufRd.ar(2, buf, Lag.ar(K2A.ar(TRand.kr(0,1,trig)*BufFrames.ir(buf)),0.1));
			kln = DynKlank.ar(`[Array.fill(16,{|i| ((i+1).pow(2))*TRand.kr(32, bw, trig)}),
				Array.fill(16,{|i| ((16-i) * (TRand.kr(0.0001, 0.003,trig)))}),
				Array.fill(16,{TRand.kr(0.0001, 1.75, trig)})], reader, freqscale: ratio.midiratio);
			kln = Limiter.ar(kln, 0.9);
			sin = SinOsc.ar((reader*([150, 300]*ratio.midiratio))+(kln*([200, 300]*ratio.midiratio)), 0, 1) * kln;

			Out.ar(0, sin*LFNoise2.kr([0.5, 0.3]).range(0,1)*env*amp);

		}).add;

		//~campana = Synth(\campana1)


		//-------------------------------------
		//------  raspadito  ------------------

		SynthDef(\raspadito, {|pos = 0.5, buf = 19, amp = 0.5, gate = 1, rate = 0.5, lag = 10, ratio = 0|
			var env, reader, out, kln, sin, trig, pointer;

			//rate = MouseX.kr(1, 50.0);
			//amp = MouseX.kr(0.001, 1.0);
			amp = Lag.kr(amp, 0.125);
			trig = Impulse.kr(rate);
			env = EnvGen.kr(Env.asr(8, 1, 8), gate, doneAction:2);
			pointer = LFNoise1.ar(rate).range(0,1)*BufFrames.kr(buf);
			reader = BufRd.ar(2, buf, Lag.ar(pointer, lag));
			kln = DynKlank.ar(`[Array.fill(16,{|i| ((i+1)*0.125)*TRand.kr(80, 1000, trig)}),
				Array.fill(16,{|i| ((16-i) * (TRand.kr(0.0001, 0.03,trig)))}),
				Array.fill(16,{TRand.kr(0.0001, 0.25, trig)})], reader, freqscale: ratio.midiratio);
			kln = Limiter.ar(kln, 0.9);
			sin = SinOsc.ar((reader*([150, 300]*ratio.midiratio))+(kln*([200, 300]*ratio.midiratio)), 0, 1) * kln;

			Out.ar(0, (sin)*env*amp);

		}).add;

		//--------------------------------------------

		SynthDef(\raspadito2, {|pos = 0.5, buf = 19, amp = 0.5, gate = 1, rate = 0.5, lag = 0.1, ratio = 0, dlyTime = 0.01, dcyTime = 1|
			var env, reader, out, kln, sin, trig, pointer;

			//pos = MouseX.kr(0.0, 1.0);
			//rate= MouseX.kr(0.10, 100.0, 2);
			//ratio = MouseY.kr(-24, 24);
			amp = Lag.kr(amp, 0.125);
			trig = Impulse.kr(rate);
			env = EnvGen.kr(Env.asr(8, 1, 8), gate, doneAction:2);
			//pointer = pos*BufFrames.kr(buf);
			//reader = BufRd.ar(2, buf, Lag.ar(pointer, lag));
			reader = Warp1.ar(2, buf, pos, ratio.midiratio, windowSize:0.5, overlaps: 8, windowRandRatio: 0.0);
			kln = DynKlank.ar(`[Array.fill(16,{|i| ((i+1)*0.125)*rrand(80,5000)}),
				Array.fill(16,{|i| (16-i) * rrand(0.0001, 0.03)}),
				Array.fill(16,{rrand(0.0001, 0.25)})], reader, freqscale: ratio.midiratio);
			//kln = CombL.ar(Limiter.ar(kln, 0.9), 1.0, MouseY.kr(0.0001, 1), MouseY.kr(0.0001, 4));
			kln = CombL.ar(Limiter.ar(kln, 0.9), 4.0, dlyTime, dcyTime);
			//sin = SinOsc.ar((reader*([150, 300]*ratio.midiratio))+(60.midicps*ratio.midiratio), 0, (0.001*kln*([(48.midicps*ratio.midiratio), (55.midicps*ratio.midiratio)])));

			Out.ar(0, ((kln*0.3)+(reader*0.7))*env*amp);

		}).add;


		//-------------------------------------
		//------  ochoBitero  -----------------

		SynthDef(\ochoBitero, {|freq = 400, amp=0.5, gate = 1, pos = 0|

			var ay, env, noise, mouseX, arr1, arr1Buf, val1, arr2, arr2Buf, val2, arr3, arr3Buf, val3;
			//var scale = Array.fill(24, {rrand(-24,24)}); // pentatonic major

			//pos = MouseX.kr(0,1);
			arr1 = Array.fill(64, {|i| 500*(cos(i* 0.35)+ cos((i-20)* 0.135) + cos((i-400)* 0.035))+500});
			arr1Buf = LocalBuf(arr1.size, 1).set(arr1);
			val1 = Index.kr(arr1Buf, pos*BufFrames.ir(arr1Buf));

			arr2 = Array.fill(64, {|i| 350*(cos(i* 0.125) - cos((i-1)* 0.15) - cos((i-3)* 0.25))+1750});
			arr2Buf = LocalBuf(arr2.size, 1).set(arr2);
			val2 = Index.kr(arr2Buf, pos*BufFrames.ir(arr2Buf));

			arr3 = Array.fill(64, {|i| 800*(sin(i* 0.05)- sin((i+3)* 0.1) - sin((i+5)* 0.2))}+1500);
			arr3Buf = LocalBuf(arr3.size, 1).set(arr3);
			val3 = Index.kr(arr3Buf, pos*BufFrames.ir(arr3Buf));

			env = EnvGen.ar(Env.asr(8, 1, 8), gate, doneAction:2);


			ay = [AY.ar(val1, val2, val3, 15, 7,
				LFNoise1.kr(1).range(0,15),
				LFNoise1.kr(0.5).range(0,15),
				LFNoise1.kr(0.33).range(0,15), 4095),
				AY.ar(val3, val1, val2, 15, 7,
					LFNoise1.kr(1).range(0,15),
					LFNoise1.kr(0.5).range(0,15),
					LFNoise1.kr(0.33).range(0,15), 4095)
			];

			Out.ar(0, ay*env*amp);

		}).add;

		//-------------------------------------
		//------  fhn  -----------------

		SynthDef(\fhn, {|out=0, pan=0, amp=0.1, pr=1, pDisp=0, tDisp=0, rate = 2, gate = 1|
			var sig, ps, kln, trig, env;

			env = EnvGen.ar(Env.asr(8, 1, 8), gate, doneAction:2);

			rate = MouseY.kr(1, 20);
			tDisp = MouseX.kr(0,4);
			pr = MouseY.kr(-12.midiratio, 0.midiratio);
			//rate = MouseY.kr(1,25);
			sig=Fhn2DC.ar(MouseX.kr(80, 3900), MouseX.kr(340,  4000), MouseY.kr(0.25, 0.9), MouseY.kr(0.15, 0.9),mul:amp);

			trig = Impulse.kr(rate);

			kln = DynKlank.ar(`[Array.fill(16,{|i| ((i+1)*0.125)*TRand.kr(80, 1000, trig)}),
				Array.fill(16,{|i| ((16-i) * (TRand.kr(0.0001, 0.03,trig)))}),
				Array.fill(16,{TRand.kr(0.0001, 0.25, trig)})], sig);

			kln = Limiter.ar(kln, 0.9);

			ps = PitchShift.ar(kln, 0.1, pr, pDisp, tDisp);

			Out.ar(0,Pan2.ar(ps, pan))
		}).add;

		//----------------------------------
		//--- prepara info para cotrolar PatternConductor ---
		r1 = [4, [1, 1.5, 0.5, 1]].convertRhythm.flatten;
		r2 = [4, [1,-3,3,2]].convertRhythm.flatten;
		r3 = [4, [1, 2, 3]].convertRhythm.flatten;
		seq2 = Pxrand([60, 63, 64, 65, 66, 67], inf).asStream;
		seq1 = Pseq([0, 1, -1, 0, 1, -1].scramble, inf).asStream;

		r4 = [4, [1.5, 1.0, -0.5, 1, 2]].convertRhythm.flatten;
		r5 = [4, [-1, 2, 3, 2, 1.5, 1.5]].convertRhythm.flatten;
		r6 = [4, [1,1,1]].convertRhythm.flatten;
		seq4 = Pxrand([60, 63, 64, 65, 66, 67], inf).asStream;
		seq3 = Pseq([0, -1, 1, 0, -1, 1].scramble, inf).asStream;

		pb1 = PbindProxy(\instrument, \che,
			\freq, ((12*seq1)+seq2).midicps,
			\dur , Pseq(r1, inf),
			\amp, 0.3);

		pb2 = PbindProxy(\instrument, \che,
			\freq, ((12*seq1)+seq2).midicps,
			\dur , Pseq(r2, inf),
			\amp, 0.3);

		pb3 = PbindProxy(\instrument, \che,
			\freq, ((12*seq1)+seq2).midicps,
			\dur , Pseq(r3, inf),
			\amp, 0.3);

		pb4 = PbindProxy(\instrument, \che,
			\freq, ((12*seq3)+seq4).midicps,
			\dur , Pseq(r4, inf),
			\amp, 0.3);

		pb5 = PbindProxy(\instrument, \che,
			\freq, ((12*seq3)+seq4).midicps,
			\dur , Pseq(r5, inf),
			\amp, 0.3);

		pb6 = PbindProxy(\instrument, \che,
			\freq, ((12*seq3)+seq4).midicps,
			\dur , Pseq(r6, inf),
			\amp, 0.3);


		//---------------------------------------------------------------------
		// ~cheS = Synth(\che, [\freq, 57.midicps, \amp, 1, \dur, 0.5])
		// ~cheS.set(\gate, 0);
		// ~fmRes1S1 = Synth(\fmRes, [\freq, 72.midicps,\dur , 0.2, \amp, 0.25]);
		// ~fmRes1S1.set(\gate, 0)
		//------------------------------------------------------------------
		//--   Tdefs  ---
		//--   Tdef skTd  ---
		parser = Tdef(\skTd, {
			var seg, delta, win1, win2, win3, win4;
			seg = 0;
			delta = 0.02;

			inf.do({

				//eskeleto.users.postln;
				eskeleto.users.do{|item, i|
					var velMI, velMD, velPI, velPD, torX, torZ, velT, soundsDic ;
					//("userID " ++ item.usrId).postln;
					//("posI"++item.manoI.pos).postln;
					//("posD"++item.manoD.pos).postln;
					//("acc"++item.pieI.acc).postln;
					//velMI = (item.manoI.vel).squared.sum.sqrt;
					//velMD = (item.manoD.vel).squared.sum.sqrt;
					velPI = (item.pieI.vel).squared.sum.sqrt;
					velPD = (item.pieD.vel).squared.sum.sqrt;
					velT = (item.torso.vel).squared.sum.sqrt;
					soundsDic = item.sounds;
					//"torso: ".post;
					//item.torso.postln;

					//item.in.postln;

					if( item.in == true, {

						if(manoIbVal == 1, {
							this.manoI(item);
							},
							{
								soundsDic.at(\cheConductorI).stop;
								soundsDic.removeAt(\cheConductorI);
						});

						if(manoDbVal == 1, {
							this.manoD(item);
							},
							{
								soundsDic.at(\cheConductorD).stop;
								soundsDic.removeAt(\cheConductorD);
						});

						if(manoITbVal == 1, {
							this.torsoManoI(item);
							},
							{
								soundsDic.at(\suckMyBufI).set(\gate, 0);
								soundsDic.removeAt(\suckMyBufI);
						});

						if(manoDTbVal == 1, {
							this.torsoManoD(item);
							},
							{
								soundsDic.at(\suckMyBufD).set(\gate, 0);
								soundsDic.removeAt(\suckMyBufD);
						});

						if(codoIbVal == 1, {
							this.codoI(item);
							},
							{
								soundsDic.at(\codoI).set(\gate, 0);
								soundsDic.removeAt(\codoI);
						});

						if(codoDbVal == 1, {
							this.codoD(item);
							},
							{
								soundsDic.at(\codoD).set(\gate, 0);
								soundsDic.removeAt(\codoD);
						});

						if(balanceVal == 1, {
							this.balance(item);
							},
							{
								soundsDic.at(\balance).set(\gate, 0);
								soundsDic.removeAt(\balance);
						});

						if(ochobitVal == 1, {
							this.ochobit(item);
							},
							{
								soundsDic.at(\ochobit).set(\gate, 0);
								soundsDic.removeAt(\ochobit);
						});


						if(rodillaIbVal == 1, {
							this.rodillaI(item);
							},
							{
								soundsDic.at(\rodillaI).set(\gate, 0);
								soundsDic.removeAt(\rodillaI);
						});

						if(rodillaDbVal == 1, {
							this.rodillaD(item);
							},
							{
								soundsDic.at(\rodillaD).set(\gate, 0);
								soundsDic.removeAt(\rodillaD);
						});

						if(chamorroIbVal == 1, {
							this.chamorroI(item);
							},
							{
								soundsDic.at(\chamorroI).set(\gate, 0);
								soundsDic.removeAt(\chamorroI);
						});

						if(chamorroDbVal == 1, {
							this.chamorroD(item);
							},
							{
								soundsDic.at(\chamorroD).set(\gate, 0);
								soundsDic.removeAt(\chamorroD);
						});

						if(pieIVal == 1, {
							this.pieI(item);
							},
							{
								soundsDic.at(\pieI).set(\gate,0);
								soundsDic.removeAt(\pieI);
						});

						if(pieDVal == 1, {
							this.pieD(item);
							},
							{
								soundsDic.at(\pieD).set(\gate,0);
								soundsDic.removeAt(\pieD);
						});

						if(pieIFmVal == 1, {
							this.pieIFm(item);
							},
							{
								soundsDic.at(\fmResI).set(\gate,0);
								soundsDic.removeAt(\fmResI);
						});

						if(pieDFmVal == 1, {
							this.pieDFm(item);
							},
							{
								soundsDic.at(\fmResD).set(\gate,0);
								soundsDic.removeAt(\fmResD);
						});

						},

						{
							// si usuario salió de escena
							if(item.in == false,
								{
									soundsDic.at(\cheConductorI).stop;
									soundsDic.removeAt(\cheConductorI);
									soundsDic.at(\cheConductorD).stop;
									soundsDic.removeAt(\cheConductorD);

									soundsDic.at(\suckMyBufI).set(\gate, 0);
									soundsDic.removeAt(\suckMyBufI);
									soundsDic.at(\suckMyBufD).set(\gate, 0);
									soundsDic.removeAt(\suckMyBufD);

									soundsDic.at(\codoI).set(\gate, 0);
									soundsDic.removeAt(\codoI);
									soundsDic.at(\codoD).set(\gate, 0);
									soundsDic.removeAt(\codoD);

									soundsDic.at(\balance).set(\gate, 0);
									soundsDic.removeAt(\balance);
									soundsDic.at(\ochobit).set(\gate, 0);
									soundsDic.removeAt(\ochobit);

									soundsDic.at(\rodillaI).set(\gate, 0);
									soundsDic.removeAt(\rodillaI);
									soundsDic.at(\rodillaD).set(\gate, 0);
									soundsDic.removeAt(\rodillaD);

									soundsDic.at(\chamorroI).set(\gate, 0);
									soundsDic.removeAt(\chamorroI);
									soundsDic.at(\chamorroD).set(\gate, 0);
									soundsDic.removeAt(\chamorroD);

									soundsDic.at(\pieI).set(\gate,0);
									soundsDic.removeAt(\pieI);
									soundsDic.at(\pieD).set(\gate,0);
									soundsDic.removeAt(\pieD);

									soundsDic.at(\fmResI).set(\gate,0);
									soundsDic.removeAt(\fmResI);
									soundsDic.at(\fmResD).set(\gate,0);
									soundsDic.removeAt(\fmResD);



							});
					});

					seg = seg + delta;

				};

				tick = tick+1;
				delta.wait;
			})
		});

		//---------------------------------------------------------
		// GUI ----------------------------------------------------
		//---------------------------------------------------------
		w = gui.window.new("Esqueletofono", Rect(30,30, 360, 650));

		manoIButton = gui.button.new(w, Rect(50, 50, 60, 40));
		manoIButton.states =[["Mano I", Color.black, Color.grey],["Mano I", Color.black, Color.red]];
		manoIButton.action = {|bt|
			if(bt.value == 1, {
				manoIbVal = 1;
			}, {manoIbVal = 0;})
		};

		manoDButton = gui.button.new(w, Rect(250, 50, 60, 40));
		manoDButton.states =[["Mano D", Color.black, Color.grey],["Mano D", Color.black, Color.red]];
		manoDButton.action = {|bt|
			if(bt.value == 1, {
				manoDbVal = 1;
			}, {manoDbVal = 0;})
		};

		manoITorsoButton = gui.button.new(w, Rect(50, 100, 60, 40));
		manoITorsoButton.states = [["manoIT", Color.black, Color.grey],["manoIT", Color.black, Color.red]];
		manoITorsoButton.action = {|bt|
			if(bt.value == 1, {
				manoITbVal = 1;
			}, {manoITbVal = 0});
		};

		manoDTorsoButton = gui.button.new(w, Rect(250, 100, 60, 40));
		manoDTorsoButton.states = [["manoDT", Color.black, Color.grey],["manoDT", Color.black, Color.red]];
		manoDTorsoButton.action = {|bt|
			if(bt.value == 1, {
				manoDTbVal = 1;
			}, {manoDTbVal = 0});
		};

		codoIButton = gui.button.new(w, Rect(50, 150, 60, 40));
		codoIButton.states = [["codoI", Color.black, Color.grey],["codoI", Color.black, Color.red]];
		codoIButton.action = {|bt|
			if(bt.value == 1, {
				codoIbVal = 1;
			}, {codoIbVal = 0});
		};

		codoDButton = gui.button.new(w, Rect(250, 150, 60, 40));
		codoDButton.states = [["codoD", Color.black, Color.grey],["codoD", Color.black, Color.red]];
		codoDButton.action = {|bt|
			if(bt.value == 1, {
				codoDbVal = 1;
			}, {codoDbVal = 0});
		};

		cabezaCaderaButton = gui.button.new(w, Rect(150, 200, 60, 40));
		cabezaCaderaButton.states = [["balance", Color.black, Color.grey],["balance", Color.black, Color.red]];
		cabezaCaderaButton.action = {|bt|
			if(bt.value == 1, {
				balanceVal = 1;
			}, {balanceVal = 0});
		};

		ochobitButton = gui.button.new(w, Rect(150, 250, 60, 40));
		ochobitButton.states = [["ochobit", Color.black, Color.grey],["ochobit", Color.black, Color.red]];
		ochobitButton.action = {|bt|
			if(bt.value == 1, {
				ochobitVal = 1;
			}, {ochobitVal = 0});
		};

		rodillaIButton = gui.button.new(w, Rect(50, 300, 60, 40));
		rodillaIButton.states = [["rodI", Color.black, Color.grey],["rodI", Color.black, Color.red]];
		rodillaIButton.action = {|bt|
			if(bt.value == 1, {
				rodillaIbVal = 1;
			}, {rodillaIbVal = 0});
		};

		rodillaDButton = gui.button.new(w, Rect(250, 300, 60, 40));
		rodillaDButton.states = [["rodD", Color.black, Color.grey],["rodD", Color.black, Color.red]];
		rodillaDButton.action = {|bt|
			if(bt.value == 1, {
				rodillaDbVal = 1;
			}, {rodillaDbVal = 0});
		};

		chamorroIButton = gui.button.new(w, Rect(50, 350, 60, 40));
		chamorroIButton.states = [["chamI", Color.black, Color.grey],["chamI", Color.black, Color.red]];
		chamorroIButton.action = {|bt|
			if(bt.value == 1, {
				chamorroIbVal = 1;
			}, {chamorroIbVal = 0});
		};
		/*		chamorroIButton2 = gui.button.new(w, Rect(120, 350, 60, 20));
		chamorroIButton2.states = [["cal", Color.black, Color.grey],["cal", Color.black, Color.red]];
		chamorroIButton.action = {|bt|
		if(bt.value == 1, {
		chamorroIb2Val = 1;
		calibrate
		}, {chamorroIb2Val = 0});
		};*/

		chamorroDButton = gui.button.new(w, Rect(250, 350, 60, 40));
		chamorroDButton.states = [["chamD", Color.black, Color.grey],["chamD", Color.black, Color.red]];
		chamorroDButton.action = {|bt|
			if(bt.value == 1, {
				chamorroDbVal = 1;
			}, {chamorroDbVal = 0});
		};

		pieIButton = gui.button.new(w, Rect(50, 400, 60, 40));
		pieIButton.states = [["pieI", Color.black, Color.grey],["pieI", Color.black, Color.red]];
		pieIButton.action = {|bt|
			if(bt.value == 1, {
				pieIVal = 1;
			}, {pieIVal = 0});
		};

		pieDButton = gui.button.new(w, Rect(250, 400, 60, 40));
		pieDButton.states = [["pieD", Color.black, Color.grey],["pieD", Color.black, Color.red]];
		pieDButton.action = {|bt|
			if(bt.value == 1, {
				pieDVal = 1;
			}, {pieDVal = 0});
		};

		pieIFmButton = gui.button.new(w, Rect(50, 450, 60, 40));
		pieIFmButton.states = [["pieIFm", Color.black, Color.grey],["pieIFm", Color.black, Color.red]];
		pieIFmButton.action = {|bt|
			if(bt.value == 1, {
				pieIFmVal = 1;
			}, {pieIFmVal = 0});
		};

		pieDFmButton = gui.button.new(w, Rect(250, 450, 60, 40));
		pieDFmButton.states = [["pieDFm", Color.black, Color.grey],["pieDFm", Color.black, Color.red]];
		pieDFmButton.action = {|bt|
			if(bt.value == 1, {
				pieDFmVal = 1;
			}, {pieDFmVal = 0});
		};

		playButton = gui.button.new(w, Rect(50, 550, 60, 40));
		playButton.states = [["play", Color.black, Color.green],["stop", Color.black, Color.red]];
		playButton.action = {|bt|
			if(bt.value == 1, {
				this.parser.play;
			}, {this.parser.stop});
		};

		laberintoButton = gui.button.new(w, Rect(250, 550, 60, 40));
		laberintoButton.states = [["laberinto", Color.black, Color.green],["stop", Color.black, Color.red]];
		laberintoButton.action = {|bt|
			if(bt.value == 1, {
				this.ponerEstado([0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0]); // uno
				escena = 1;
				this.laberinto.play(AppClock);
				}, {
					this.ponerEstado([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]); // cero
					escena = 0;
					this.laberinto.stop});
		};

		// set start button to zero upon a cmd-period
		cmdPeriodFunc = {
			manoIButton.value = 0; manoIbVal = 0;
			manoDButton.value = 0; manoDbVal = 0;
			manoITorsoButton.value = 0; manoITbVal = 0;
			manoDTorsoButton.value = 0; manoDTbVal = 0;
			pieIButton.value = 0; pieIVal = 0;
			pieDButton.value = 0; pieDVal = 0;
			pieIFmButton.value = 0; pieIFmVal = 0;
			pieDFmButton.value = 0; pieDFmVal = 0;
			rodillaIButton.value = 0; rodillaIbVal = 0;
			rodillaDButton.value = 0; rodillaDbVal = 0;
			chamorroIButton.value = 0; chamorroIbVal = 0;
			chamorroDButton.value = 0; chamorroDbVal = 0;
			codoIButton.value = 0; codoIbVal = 0;
			codoDButton.value = 0; codoDbVal = 0;
			manoITorsoButton.value = 0; manoITbVal = 0;
			manoDTorsoButton.value = 0; manoDTbVal = 0;
			cabezaCaderaButton.value = 0; balanceVal = 0;
			ochobitButton = 0; ochobitVal = 0; escena = 0;
		};

		CmdPeriod.add(cmdPeriodFunc);

		// stop the sound when window closes and remove cmdPeriodFunc.
		w.onClose = {
			parser.stop;
			CmdPeriod.remove(cmdPeriodFunc);
		};

		w.front;

		//this.calibrate(6);


		//--------------------------------
		//-------- laberintoTD -----------
		//--------------------------------

		laberinto = Tdef(\laberinto, {
			var seg, delta, state, lab, torson, posInLab, ready;
			posInLab = [0,0];
			ready = true;
			lab = Dictionary.new();
			lab.add(\uno->   [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0]);
			lab.add(\dos->   [0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1]);
			lab.add(\tres->  [0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0]);
			lab.add(\cuatro->[0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0]);
			seg = 0;
			delta = 0.1;

			inf.do({
				seg = seg + delta;
				//eskeleto.users.postln;

				if( this.parser.isPlaying, {
					"si".postln;
					"usuarios: ".post;
					cinderAddr.sendMsg("/escena", escena.asFloat);
					cinderAddr2.sendMsg("/escena", escena.asFloat);
					this.eskeleto.users.postln;
					this.eskeleto.users.do{|item, i|
						"torson: ".post;
						torson = item.torso.pos.postln;

						//-- Move forward on z-axis
						if((torson[2] < 1500.0 && posInLab == [0,0] && ready),
							{
								this.ponerEstado(lab.at(\cuatro));
								escena = 4;
								"posInLab: ".post;
								posInLab = [0,1].postln;
								ready = false;
						});

						if((torson[2] < 1500.0 && posInLab == [0,1] && ready),
							{
								this.ponerEstado(lab.at(\tres));
								escena = 3;
								"posInLab: ".post;
								posInLab = [0,-2].postln;
								ready = false;
						});

						if((torson[2] < 1500.0 && posInLab == [0,-2] && ready),
							{
								this.ponerEstado(lab.at(\dos));
								escena = 2;
								"posInLab: ".post;
								posInLab = [0,-1].postln;
								ready = false;
						});

						if((torson[2] < 1500.0 && posInLab == [0,-1] && ready),
							{
								this.ponerEstado(lab.at(\uno));
								escena = 1;
								"posInLab: ".post;
								posInLab = [0,0].postln;
								ready = false;
						});

						//-- Move backwards on z-axis
						if((torson[2] > 3200.0 && posInLab == [0,0] && ready),
							{
								this.ponerEstado(lab.at(\dos));
								escena = 2;
								"posInLab: ".post;
								posInLab = [0,-1].postln;
								ready = false;
						});

						if((torson[2] > 3200.0 && posInLab == [0,-1] && ready),
							{
								this.ponerEstado(lab.at(\cuatro));
								escena = 4;
								"posInLab: ".post;
								posInLab = [0,1].postln;
								ready = false;
						});

						if((torson[2] > 3200.0 && posInLab == [0,1] && ready),
							{
								this.ponerEstado(lab.at(\uno));
								escena = 1;
								"posInLab: ".post;
								posInLab = [0,0].postln;
								ready = false;
						});

						//-- Simple debounce by declaration of reactive zone
						if((torson[2] < 3200.0) && (torson[2] > 1500.0),
							{
								ready = true;
						});

						//-- Move left
						if((torson[0] < -1500.0 && posInLab == [0,0] && ready),
							{
								this.ponerEstado(lab.at(\tres));
								escena = 3;
								"posInLab: ".post;
								posInLab = [-1,0].postln;
								ready = false;
						});

						if((torson[0] < -1500.0 && posInLab == [-1,0] && ready),
							{
								this.ponerEstado(lab.at(\dos));
								escena = 2;
								"posInLab: ".post;
								posInLab = [1, 0].postln;
								ready = false;
						});

						if((torson[0] < -1500.0 && posInLab == [1,0] && ready),
							{
								this.ponerEstado(lab.at(\uno));
								escena = 1;
								"posInLab: ".post;
								posInLab = [0,0].postln;
								ready = false;
						});

						//-- Move right
						if((torson[0] < -1500.0 && posInLab == [0,0] && ready),
							{
								this.ponerEstado(lab.at(\dos));
								escena = 2;
								"posInLab: ".post;
								posInLab = [-1,0].postln;
								ready = false;
						});

						if((torson[0] < -1500.0 && posInLab == [-1,0] && ready),
							{
								this.ponerEstado(lab.at(\tres));
								escena = 3;
								"posInLab: ".post;
								posInLab = [1, 0].postln;
								ready = false;
						});

						if((torson[0] < -1500.0 && posInLab == [1,0] && ready),
							{
								this.ponerEstado(lab.at(\uno));
								escena = 1;
								"posInLab: ".post;
								posInLab = [0,0].postln;
								ready = false;
						});

						//-- Simple debounce by declaration of reactive zone
						if((torson[0] < -1500.0) && (torson[0] > 1500.0),
							{
								ready = true;
						});

					};
					},
					{"no".postln});


				delta.wait;

			});
		});

	}
	//--------------------------------
	//---------  ponerEstado  --------
	//--------------------------------
	ponerEstado{|estado|
		if(estado.size == 16, {

			manoIButton.value = estado[0];
			manoIbVal = estado[0];
			manoDButton.value = estado[1];
			manoDbVal = estado[1];
			manoITorsoButton.value = estado[2];
			manoITbVal = estado[2];
			manoDTorsoButton.value = estado[3];
			manoDTbVal = estado[3];
			codoIButton.value = estado[4];
			codoIbVal = estado[4];
			codoDButton.value = estado[5];
			codoDbVal = estado[5];
			rodillaIButton.value = estado[6];
			rodillaIbVal = estado[6];
			rodillaDButton.value = estado[7];
			rodillaDbVal = estado[7];
			chamorroIButton.value = estado[8];
			chamorroIbVal = estado[8];
			chamorroDButton.value = estado[9];
			chamorroDbVal = estado[9];
			pieIButton.value = estado[10];
			pieIVal = estado[10];
			pieDButton.value = estado[11];
			pieDVal = estado[11];
			pieIFmButton.value = estado[12];
			pieIFmVal = estado[12];
			pieDFmButton.value = estado[13];
			pieDFmVal = estado[13];
			cabezaCaderaButton.value = estado[14];
			balanceVal = estado[14];
			ochobitButton.value = estado[15];
			ochobitVal = estado[15];

		});
	}


	//--------------------------------
	//---------   chePlayer   --------
	//--------------------------------

	chePlayerI{//arg vel, posy;

		var conductor;

		^conductor = PatternConductor([pb1, pb2, pb3], quant:1);

	}

	//--------------------------------
	//---------   chePlayerD   -------
	//--------------------------------

	chePlayerD{//arg vel, posy;

		var conductor;

		^conductor = PatternConductor([pb4, pb5, pb6], quant:1);

	}
	//--------------------------------
	//---------  ManoI (chePlayerD) --
	//--------------------------------

	manoI{|sk|
		var soundsDic, velMI, temps;


		temps = [0.125, 0.5, 0.75, 1, 2, 3, 4, 6, 8, 12, 8, 24, 32];
		velMI = (sk.manoI.vel).squared.sum.sqrt;
		"vel MI: ".postln;
		velMI.postln;

		soundsDic = sk.sounds;
		if(soundsDic.at(\cheConductorI) != nil, {

			soundsDic.at(\cheConductorI).patterns.do({|pbProxy, indx|
				//item.manoI.pos[1].postln;
				//(item.torso.pos[2]-item.manoI.pos[2]).postln;
				//((item.torso.pos[2]-item.manoI.pos[2]).abs).linlin(0,500, 1, 50).postln;
				//pitch = sk.manoI.pos[1].linlin(-750, 750, -20, 60);
				pitch = velMI.linlin(0.0, 250.0, -36, 60);
				//pbProxy.set(\bw, ((item.torso.pos[2]-item.manoI.pos[2]).abs).linlin(0,500, 1, 50));
				//pbProxy.set(\freq, ((pitch*seq1.next)+seq2.next).midicps);
				pbProxy.set(\freq, ((pitch)+seq2.next).midicps);
				pbProxy.set(\amp, velMI.linlin(30.0, 250.0, 0.0, 1.5) * Pxrand([1.0, 0.5, 0.7, 0.7, 1.0],inf));
				pbProxy.set(\dur, Pxrand(r4.rotate(indx*2), inf));
				//pbProxy.postln;
			});

			soundsDic.at(\cheConductorI).tempo_((velMI.linlin(0.0, 250.0, 1, 6)).asInt * 4);
			//soundsDic.at(\cheConductorI).tempo_(temps[(velMI.linexp(0, 150, 0.5,12)).asInteger]);

			}, {
				soundsDic.add(\cheConductorI -> this.chePlayerI);
				soundsDic.at(\cheConductorI).play;
		});
	}

	//--------------------------------
	//---------   mano D (chePlayerD)
	//--------------------------------

	manoD{|sk|
		var soundsDic, velMD;

		soundsDic = sk.sounds;
		velMD = (sk.manoD.vel).squared.sum.sqrt;
		"vel MD: ".postln;
		velMD.postln;

		if(soundsDic.at(\cheConductorD) != nil, {
			soundsDic.at(\cheConductorD).patterns.do({|pbProxy, indx|
				//item.manoD.pos[1].postln;
				//pitch = sk.manoD.pos[1].linlin(-750, 750, -20, 60);
				pitch = velMD.linlin(30.0, 250.0, -36, 60);
				//(item.torso.pos[2]-item.manoD.pos[2]).postln;

				//pbProxy.set(\bw, ((item.torso.pos[2]-item.manoD.pos[2]).abs).linlin(0,500, 1, 50));
				//pbProxy.set(\freq, ((pitch*seq3.next)+seq4.next).midicps);
				//pbProxy.set(\amp, velMD.linlin(0.0, 200.0, 0.000, 1.0));
				pbProxy.set(\freq, ((pitch)+seq2.next).midicps);
				pbProxy.set(\amp, velMD.linlin(10.0, 250.0, 0.0, 1.5) * Pxrand([1.0, 0.5, 0.7, 0.7, 1.0],inf));
				pbProxy.set(\dur, Pxrand(r5.rotate(indx*2), inf));
				//pbProxy.postln;
			});

			soundsDic.at(\cheConductorD).tempo_((velMD.linexp(0.0, 250, 1, 6)).asInt*4);

			}, {
				soundsDic.add(\cheConductorD -> this.chePlayerD);
				soundsDic.at(\cheConductorD).play;
		});

	}

	//----------------------------------------------------------
	torsoManoI{|sk|

		var soundsDic, torX, torZ ;
		var pHombroMano, angMano, ang, phi;
		soundsDic = sk.sounds;

		if( soundsDic.at(\suckMyBufI) != nil,
			//if( item.in == true,
			{
				//limit torX's range and map from 0 to 1
				//torX = [-1250,1250].asSpec.unmap(sk.torso.pos[0]);
				//torX;
				//torZ = sk.torso.pos[2].linlin(1001, 3000, 0.25, 0.75);
				//((sk.torso.pos[1]-sk.manoI.pos[1]).abs);
				//((sk.torso.pos-sk.manoI.pos).squared.sum.sqrt);

				pHombroMano = Cartesian(sk.manoI.pos[0]-sk.hombroI.pos[0],
					sk.manoI.pos[1]-sk.hombroI.pos[1],
					sk.manoI.pos[2]-sk.hombroI.pos[2]);
				//"angulo: ".post;
				angMano = pHombroMano.theta;
				phi = pHombroMano.phi;
				if(angMano < 0, {ang = (2*pi)+angMano;}, {ang = angMano});

				//"anguloAbs: ".post;
				//ang.postln;

				//"amp Val: ".post;
				ang.linlin(pi/2, 3*pi/2, 1.0, 0.0);

				soundsDic.at(\suckMyBufI).set(
					//\prate, (torX.linlin(0,1, -12, 24).asInteger).midiratio,
					\prate, ((sk.torso.pos[2]-sk.manoI.pos[2]).abs).linlin(0,500, 0.0, 1),
					//\ampUno, velT.linexp(10, 300,0.01, 0.75 ),
					//\ampUno, ((item.torso.pos[1]-item.manoI.pos[1]).abs).linlin(0,800, 0.0, 1),
					//\ampUno, ang.linlin(pi/2, 3*pi/2, 1.0, 0.0),
					\ampUno, sin(ang).linlin(-1, 1, 0.0, 0.80),
					\pos1, cos(phi).linlin(-1, 1, 0.0, 1.0),
					\pos2, sin(phi).linlin(-1, 1, 0.0, 1.0),
					\dur1, ((sk.torso.pos[0]).abs).linlin(0,1250, 0.75, 0.125),
					\dur2, ((sk.torso.pos[0]).abs).linlin(0,1250, 0.75, 0.125));

				cinderAddr.sendMsg("/torsoManoI", sin(ang).linlin(-1, 1, 0.0, 127.0), sin(phi).linlin(-1, 1, 0.0, 127.0));
				cinderAddr2.sendMsg("/torsoManoI", 1-sin(ang).linlin(-1, 1, 0.0, 127.0), 1-sin(phi).linlin(-1, 1, 0.0, 1.0));
			},

			{
				soundsDic.add(\suckMyBufI -> Synth(\suckMyGrainBuff,
					[\prate, -12.midiratio, \trate, 5.25,
						\ampUno, 0.1, \pos1, 0.25, \pos2, 0.75,
						\dur1, 0.125, \dur2, 0.125, \buf1, this.pizBuf1,
						\buf2, this.chaBuf2]))
		});


	}

	//----------------------------------------------------------
	torsoManoD{|sk|

		var soundsDic, torX, torZ ;
		var pHombroMano, angMano, ang, phi;
		soundsDic = sk.sounds;

		if( soundsDic.at(\suckMyBufD) != nil,
			//if( item.in == true,
			{

				torX = [-1250,1250].asSpec.unmap(sk.torso.pos[0]);
				torX;
				torZ = sk.torso.pos[2].linlin(1001, 3000, 0.25, 0.75);
				((sk.torso.pos[1]-sk.manoD.pos[1]).abs);
				((sk.torso.pos-sk.manoD.pos).squared.sum.sqrt);

				pHombroMano = Cartesian(sk.manoD.pos[0]-sk.hombroD.pos[0],
					sk.manoD.pos[1]-sk.hombroD.pos[1],
					sk.manoD.pos[2]-sk.hombroD.pos[2]);
				//"angulo: ".post;
				angMano = pHombroMano.theta;
				phi = pHombroMano.phi;

				if(angMano < 0, {ang = (2*pi)+angMano;}, {ang = angMano});

				//"anguloAbs: ".post;
				//ang.postln;

				//"amp Val: ".post;
				ang.linlin(pi/2, 3*pi/2, 1.0, 0.0);

				soundsDic.at(\suckMyBufD).set(
					//\prate, (torX.linlin(0,1, -12, 24).asInteger).midiratio,
					\prate, ((sk.torso.pos[2]-sk.manoD.pos[2]).abs).linlin(0, 500, 0.0, 1),
					//\ampUno, velT.linexp(10, 300,0.01, 0.75 ),
					//\ampUno, ((item.torso.pos[1]-item.manoI.pos[1]).abs).linlin(0,800, 0.0, 1),
					//\ampUno, ang.linlin(pi/2, 3*pi/2, 1.0, 0.0),
					\ampUno, sin(ang).linlin(-1, 1, 0.0, 0.80),
					\pos1, cos(phi).linlin(-1, 1, 0.0, 127.0),
					\pos2, sin(phi).linlin(-1, 1, 0.0, 127.0),
					\dur1, ((sk.torso.pos[0]).abs).linlin(0,1250, 0.75, 0.125),
					\dur2, ((sk.torso.pos[0]).abs).linlin(0,1250, 0.75, 0.125));

				cinderAddr.sendMsg("/torsoManoD", sin(ang).linlin(-1, 1, 0.0, 127.0), sin(phi).linlin(-1, 1, 0.0, 127.0));
				cinderAddr2.sendMsg("/torsoManoD", 1-sin(ang).linlin(-1, 1, 0.0, 127.0), sin(phi).linlin(-1, 1, 0.0, 1.0));
			},

			{
				soundsDic.add(\suckMyBufD -> Synth(\suckMyGrainBuff,
					[\prate, -12.midiratio, \trate, 5.25,
						\ampUno, 0.1, \pos1, 0.25, \pos2, 0.75,
						\dur1, 0.125, \dur2, 0.125, \buf1, this.pizBuf1,
						\buf2, this.chaBuf2]))
		});


	}

	//---------------------------------------
	//---------  CodoI   ------------------
	//---------------------------------------
	codoI{|sk|
		var soundsDic, codo, mano, hombro, phi, antebrazo, conejo, ang, distManoHombro;
		soundsDic = sk.sounds;

		if( soundsDic.at(\codoI) != nil,
			{
				hombro = Cartesian(sk.hombroI.pos[0], sk.hombroI.pos[1], sk.hombroI.pos[2]);
				codo = Cartesian(sk.codoI.pos[0], sk.codoI.pos[1], sk.codoI.pos[2]);
				mano = Cartesian(sk.manoI.pos[0], sk.manoI.pos[1], sk.manoI.pos[2]);
				antebrazo = mano-codo;
				conejo = hombro-codo;
				// Formula para sacar el angulo entre dos vectores
				// a=Cartesian(2,-3,4);
				// b=Cartesian(5,2,1);
				// p=a*b
				// ~sum = p.asArray.sum;
				// ~ang = acos(~sum/(a.rho * b.rho));
				ang = acos(((antebrazo*conejo).asArray.sum)/(antebrazo.rho*conejo.rho));
				"ang: ".post;
				ang.postln;
				"distMH: ".post;
				distManoHombro = (mano.dist(hombro)).abs.postln;

				soundsDic.at(\codoI).set(
					\amp, distManoHombro.linlin(80, 500, 0, 0.3),
					\rate, (ang.abs).linexp(0, pi, 1, 50),
					\ratio, (ang.abs).linlin(0, pi, -60, 0),
					\bw,  (ang.abs).linlin(0, pi, 50, 500)
				);
			}, {
				soundsDic.add(\codoI -> Synth(\campana1, [\amp, 0, \rate, 1, \ratio, 0]))
		});

	}

	//---------------------------------------
	//---------  CodoD   ------------------
	//---------------------------------------
	codoD{|sk|
		var soundsDic, codo, mano, hombro, phi, antebrazo, conejo, ang, distManoHombro;
		soundsDic = sk.sounds;

		if( soundsDic.at(\codoD) != nil,
			{
				hombro = Cartesian(sk.hombroD.pos[0], sk.hombroD.pos[1], sk.hombroD.pos[2]);
				codo = Cartesian(sk.codoD.pos[0], sk.codoD.pos[1], sk.codoD.pos[2]);
				mano = Cartesian(sk.manoD.pos[0], sk.manoD.pos[1], sk.manoD.pos[2]);
				antebrazo = mano-codo;
				conejo = hombro-codo;
				// Formula para sacar el angulo entre dos vectores
				// a=Cartesian(2,-3,4);
				// b=Cartesian(5,2,1);
				// p=a*b
				// ~sum = p.asArray.sum;
				// ~ang = acos(~sum/(a.rho * b.rho));
				ang = acos(((antebrazo*conejo).asArray.sum)/(antebrazo.rho*conejo.rho));
				"ang: ".post;
				ang.postln;
				"distMH: ".post;
				distManoHombro = (mano.dist(hombro)).abs.postln;

				soundsDic.at(\codoD).set(
					\amp, 0.5-distManoHombro.explin(60, 500, 0, 0.5),
					\rate, 31-(ang.abs).linexp(0.5, pi, 1, 30),
					\ratio, (ang.abs).linlin(0.5, pi, -40, 0),
					\bw,  (ang.abs).linlin(0.5, pi, 10, 500)
				);
			}, {
				soundsDic.add(\codoD -> Synth(\campana1, [\amp, 0, \rate, 1, \ratio, 0]))
		});

	}


	//---------------------------------------
	//---------  Balance   ------------------
	//---------------------------------------
	balance{|sk|

		var soundsDic,hombroD, hombroI, cuello, caderaD, caderaI, pan8, balance, phi, theta, mag, vel;
		var torVel, torRho;
		soundsDic = sk.sounds;


		if( soundsDic.at(\balance) != nil,
			{
				caderaD = Cartesian(sk.caderaD.pos[0], sk.caderaD.pos[1],sk.caderaD.pos[2]);
				caderaI = Cartesian(sk.caderaI.pos[0], sk.caderaI.pos[1],sk.caderaI.pos[2]);
				pan8 = caderaI + ((caderaD - caderaI)/2);

				hombroD = Cartesian(sk.hombroD.pos[0], sk.hombroD.pos[1],sk.hombroD.pos[2]);
				hombroI = Cartesian(sk.hombroI.pos[0], sk.hombroI.pos[1],sk.hombroI.pos[2]);
				cuello = hombroI + ((hombroD - hombroI)/2);

				torVel = Cartesian(sk.torso.vel[0], sk.torso.vel[2],sk.torso.vel[2]);
				torRho = torVel.rho;

				//"balance: ".post;
				balance = cuello-pan8;
				//cuello-pan8.postln;
				"phi :".post;
				if(balance.rho != 0, {
				phi = acos(balance.y/balance.rho);
					phi.postln;
					}, {
						phi = 0;
				});

				/*"cos(theta): ".post;
				cos(theta).postln;*/
				//"mag :".post;
				//mag = balance.rho.postln;
				"theta :".post;
				if(balance.z != 0,
					{
						theta = atan(balance.x/balance.z);
						theta.postln;
					}, {
						theta = 0;
				});
				// "cos(phi) :".post;
				// cos(phi).postln;
				//vel = (sk.cabeza.vel).squared.sum.sqrt.postln;

				cinderAddr.sendMsg("/torso", sk.torso.pos[0].asFloat, sk.torso.pos[2].asFloat, torRho.linlin(0.0, 150.0, 0.0, 1.0).asFloat);
				cinderAddr2.sendMsg("/torso", sk.torso.pos[0].asFloat, sk.torso.pos[2].asFloat, torRho.linlin(0.0, 150.0, 0.0, 1.0).asFloat);

				//				soundsDic.at(\balance).set(\pos, cos(phi).abs,
				//					\amp, cos(theta).abs);
				//				soundsDic.at(\balance).set(\pos, theta.linlin(0, pi, 0.0, 1.0),
				//					\amp, (phi.abs).linlin(0.0, pi/2, 0.0, 1.0));
				soundsDic.at(\balance).set(\pos, theta.linlin(0, pi, 0.0, 1.0),
					\amp, (phi.abs).linlin(0.0, pi/2, 0.0, 1.0));

			},
			{
				soundsDic.add(\balance -> Synth(\procesDance, [\preLevel, 0.75, \amp, 0.50, \gate, 1, \pos, 0.5, \grSize, 0.125, \ovr, 25, \pan, 0, \wrr, 0.75]));
		});


	}

	//---------------------------------------
	//---------  Ochobit   ------------------
	//---------------------------------------
	ochobit{|sk|

		var soundsDic,hombroD, hombroI, cuello, caderaD, caderaI, pan8, balance, phi, theta,plano, mag, vel, torVel, torRho;

		soundsDic = sk.sounds;


		if( soundsDic.at(\ochobit) != nil,
			{
				caderaD = Cartesian(sk.caderaD.pos[0], sk.caderaD.pos[1],sk.caderaD.pos[2]);
				caderaI = Cartesian(sk.caderaI.pos[0], sk.caderaI.pos[1],sk.caderaI.pos[2]);
				pan8 = caderaI + ((caderaD - caderaI)/2);

				hombroD = Cartesian(sk.hombroD.pos[0], sk.hombroD.pos[1],sk.hombroD.pos[2]);
				hombroI = Cartesian(sk.hombroI.pos[0], sk.hombroI.pos[1],sk.hombroI.pos[2]);
				cuello = hombroI + ((hombroD - hombroI)/2);

				plano = Point(cuello.x, cuello.z-2500);
				//"plano.rho: ".post;
				mag = plano.rho;


				vel = Cartesian(sk.hombroD.vel[0], sk.hombroD.vel[1], sk.hombroD.vel[2]);
				torVel = Cartesian(sk.torso.vel[0], sk.torso.vel[2],sk.torso.vel[2]);
				torRho = torVel.rho;

				//"balance: ".post;
				//balance = cuello-pan8.postln;
				//"theta :".post;
				//theta = balance.theta;
				//theta = balance.theta;

				/*"cos(theta): ".post;
				cos(theta).postln;*/
				//"mag :".post;
				//mag = balance.rho.postln;
				//"phi :".post;
				//phi = balance.phi.postln;
				// "cos(phi) :".post;
				// cos(phi).postln;
				//vel = (sk.cabeza.vel).squared.sum.sqrt.postln;
				"planoTheta: ".post;
				plano.theta.postln;

				"torso: ".post; sk.torso.postln;
				cinderAddr.sendMsg("/torso", sk.torso.pos[0].asFloat, sk.torso.pos[2].asFloat, torRho.linlin(0.0, 150.0, 0.0, 1.0).asFloat);
				cinderAddr2.sendMsg("/torso", sk.torso.pos[0].asFloat, sk.torso.pos[2].asFloat, torRho.linlin(0.0, 150.0, 0.0, 1.0).asFloat);
				cinderAddr.sendMsg("/cuelloAngulo", plano.theta.linlin(-pi, pi, 60.0, 127.0).asFloat);
				cinderAddr2.sendMsg("/cuelloAngulo", plano.theta.linlin(-pi, pi, 60.0, 127.0).asFloat);

				cinderAddr.sendMsg("/manoI", sk.manoI.pos[0].asFloat, sk.manoI.pos[1].asFloat);
				cinderAddr2.sendMsg("/manoI", sk.manoI.pos[0].asFloat, sk.manoI.pos[1].asFloat);
				cinderAddr.sendMsg("/manoD", sk.manoD.pos[0].asFloat, sk.manoD.pos[1].asFloat);
				cinderAddr2.sendMsg("/manoD", sk.manoD.pos[0].asFloat, sk.manoD.pos[1].asFloat);

				soundsDic.at(\ochobit).set(\pos, mag.linlin(0,2100, 0, 1),
					\amp, vel.rho.linlin(30, 100, 0.35, 1));

			},
			{
				soundsDic.add(\ochobit -> Synth(\ochoBitero, [\amp, 0.50, \gate, 1, \pos, 0.5]));
		});


	}




	//---------------------------------------
	//---------  RodillaI  ------------------
	//---------------------------------------

	rodillaI{|sk|
		var soundsDic, rodilla, cadera, vector, phi, theta, velMag, vel;

		soundsDic = sk.sounds;

		if(soundsDic.at(\rodillaI) != nil,{
			rodilla = Cartesian(sk.rodillaI.pos[0], sk.rodillaI.pos[1], sk.rodillaI.pos[2]);
			vel = Cartesian(sk.rodillaI.vel[0], sk.rodillaI.vel[1], sk.rodillaI.vel[2]);
			cadera = Cartesian(sk.caderaI.pos[0], sk.caderaI.pos[1], sk.caderaI.pos[2]);
			//TODO calibrate rodilla.y
			vector = rodilla-cadera;
			velMag = vel.rho;
			phi = vector.phi;
			//"rodilla: ".post; rodilla.postln; " cadera: ".post; cadera.postln;
			//"phi, cos(phi)".post;
			//phi.post; ", ".post; cos(phi).postln;
			theta = vector.theta;

			soundsDic.at(\rodillaI).set(\ratio, rodilla.y.linexp(-800, 100, -1, -30),
				\pos, cos(phi).abs, \amp, cos(theta).abs*velMag.explin(0.1,200, 0.25, 4), \pDisp, 0.25*sin(phi).abs, \tDisp, 0.3*sin(theta));

			}, {
				soundsDic.add(\rodillaI -> Synth(\ordilla, [\ratio, -12, \pos, 0, \amp, 0]));
		})

	}

	//---------------------------------------
	//---------  RodillaD  ------------------
	//---------------------------------------

	rodillaD{|sk|
		var soundsDic, rodilla, cadera, vector, phi, theta, velMag, vel;

		soundsDic = sk.sounds;

		if(soundsDic.at(\rodillaD) != nil,{
			rodilla = Cartesian(sk.rodillaD.pos[0], sk.rodillaD.pos[1], sk.rodillaD.pos[2]);
			vel = Cartesian(sk.rodillaD.vel[0], sk.rodillaD.vel[1], sk.rodillaD.vel[2]);
			cadera = Cartesian(sk.caderaD.pos[0], sk.caderaD.pos[1], sk.caderaD.pos[2]);
			//TODO calibrate rodilla.y
			vector = rodilla-cadera;
			//"mag: ".post;
			velMag = vel.rho;
			phi = vector.phi;
			//"rodilla: ".post; rodilla.postln; " cadera: ".post; cadera.postln;
			//"phi, cos(phi)".post;
			//phi.post; ", ".post; cos(phi).postln;
			theta = vector.theta;

			soundsDic.at(\rodillaD).set(\ratio, rodilla.y.linexp(-800, 100, -4, -34),
				\pos, cos(phi).abs, \amp, cos(theta).abs*velMag.explin(0.1,200, 0.25, 4), \pDisp, 0.25*sin(phi).abs, \tDisp, 0.3*sin(theta));

			}, {
				soundsDic.add(\rodillaD -> Synth(\ordilla, [\ratio, -12, \pos, 0, \amp, 0]));
		})

	}

	//---------------------------------------
	//---------  ChamorroI  -----------------
	//---------------------------------------

	chamorroI{|sk|
		var theta, soundsDic, rodilla, mano, vel, vector, plano, mag, rho;
		var hombro, vecHM;
		soundsDic = sk.sounds;

		if(soundsDic.at(\chamorroI) != nil,
			{
				mano = Cartesian(sk.manoI.pos[0], sk.manoI.pos[1],sk.manoI.pos[2]);
				rodilla = Cartesian(sk.rodillaI.pos[0], sk.rodillaI.pos[1],sk.rodillaI.pos[2]);
				vel = Cartesian(sk.manoI.vel[0], sk.manoI.vel[1],sk.manoI.vel[2]);
				hombro = Cartesian(sk.hombroI.pos[0], sk.hombroI.pos[1], sk.hombroI.pos[2]);
				vecHM = hombro-mano;
				//"vel.rho: ".post;
				rho = vel.rho;

				//"plano: ".post;
				plano = Point(mano.x, mano.z-2000);

				//"rodilla: ".postln;
				//rodilla.postln; //[-640, 300]

				//"theta".post;
				theta = vecHM.theta;

				//"mag".post;
				mag = plano.rho;

				//"mano: ".post;
				//mano.postln;

				if(rho > 100, {
					cinderAddr.sendMsg("/velPerlinI", rho.linlin(100.0, 300.0, 0.0, 1.0).asFloat);
					cinderAddr2.sendMsg("/velPerlinI", rho.linlin(100.0, 300.0, 0.0, 1.0).asFloat);
				});

				soundsDic.at(\chamorroI).set(
					//\rate, mag.linlin(100, 1500, 0.125, 25)*vel.rho.linlin(30, 100, 0.125, 4),
					\rate, rho.linlin(0.0, 300, 0.125, 100),
					//\rate, rho.linexp(inMin, inMax, outMin, outMax),
					\amp, rho.linexp(20.00, 200, 0.250, 0.8),
					\pos,  vecHM.theta.linlin(-pi, pi, 0.0, 1),
					\ratio,  rho.linlin(50.0, 150.0, -36.0, 48.0),
					//\dlyTime, 1-mag.linexp(100, 1500, 0.0001, 0.9999),
					\dlyTime, 0.01,
					\dcyTime, mag.linexp(100, 1500, 0.0001, 4.0001)
				);

				cinderAddr.sendMsg("/manoI", mano.x.asFloat, mano.y.asFloat);
				cinderAddr2.sendMsg("/manoI", mano.x.asFloat, mano.y.asFloat);
			},{

				soundsDic.add(\chamorroI -> Synth(\raspadito2, [\amp,0,\rate,1,\lag, 250, \lag, 5, \dlyTime, 1.0, \dcyTime, 1.0, \pos, 0.5]));
		});


	}

	//---------------------------------------
	//---------  ChamorroD  -----------------
	//---------------------------------------

	chamorroD{|sk|
		var theta, soundsDic, rodilla, mano, vel, vector, plano, mag, rho;
		var hombro, vecHM;

		soundsDic = sk.sounds;

		if(soundsDic.at(\chamorroD) != nil,
			{
				mano = Cartesian(sk.manoD.pos[0], sk.manoD.pos[1],sk.manoD.pos[2]);
				rodilla = Cartesian(sk.rodillaD.pos[0], sk.rodillaD.pos[1],sk.rodillaD.pos[2]);
				vel = Cartesian(sk.manoD.vel[0], sk.manoD.vel[1],sk.manoD.vel[2]);
				hombro = Cartesian(sk.hombroD.pos[0], sk.hombroD.pos[1], sk.hombroD.pos[2]);
				vecHM = hombro-mano;

				//"vel.rho: ".post;
				rho = vel.rho;

				//"plano: ".post;
				plano = Point(mano.x, mano.z-2000);

				//"planoTheta: ".postln;
				theta = plano.theta;
				//(plano.theta/(2*pi)).postln;

				//"mag".post;
				mag = plano.rho;

				//"mano: ".post;
				//mano.postln;

				if(rho > 100, {
					cinderAddr.sendMsg("/velPerlinD", rho.linlin(100.0, 300.0, 0.0, 1.0).asFloat);
					cinderAddr2.sendMsg("/velPerlinD", rho.linlin(100.0, 300.0, 0.0, 1.0).asFloat);
				});


				soundsDic.at(\chamorroD).set(
					//\rate, mag.linlin(100, 1500, 0.125, 25)*vel.rho.linlin(30, 100, 0.125, 4),
					\rate, rho.linlin(0.0, 300, 0.125, 100),
					//\rate, rho.linexp(inMin, inMax, outMin, outMax),
					\amp, rho.linexp(20.00, 200, 0.250, 0.8),
					\pos,  vecHM.theta.linlin(-pi, pi, 0.0, 1),
					\ratio,  rho.linlin(50.0, 150.0, -36.0, 48.0),
					//\dlyTime, 1-mag.linexp(100, 1500, 0.0001, 0.9999),
					\dlyTime, 0.01,
					\dcyTime, mag.linexp(100, 1500, 0.0001, 4.0001)
				);
				cinderAddr.sendMsg("/manoD", mano.x.asFloat, mano.y.asFloat);
				cinderAddr2.sendMsg("/manoD", mano.x.asFloat, mano.y.asFloat);
			},{

				soundsDic.add(\chamorroD -> Synth(\raspadito2, [\amp,0,\rate,1,\lag, 250, \lag, 5, \dlyTime, 1.0, \dcyTime, 1.0, \pos, 0.5]));
		});


	}

	//---------------------------------------
	//---------  PieI  ------------------
	//---------------------------------------

	pieI{|sk|
		var soundsDic, pieP, pieVel, tor, dtp;
		soundsDic = sk.sounds;

		if( soundsDic.at(\pieI) != nil,
			{

				//fmFreq = Pbrown()asStream.next;
				//tor = Cartesian(sk.torso.pos[0], sk.torso.pos[1],sk.torso.pos[2]);
				pieP = Cartesian(sk.pieI.pos[0], sk.pieI.pos[1],sk.pieI.pos[2]);
				pieVel = Cartesian(sk.pieI.vel[0], sk.pieI.vel[1],sk.pieI.vel[2]);
				pieVel.rho.postln;
				if(pieVel.rho > 10, {//Colocar umbral de movimiento en la interfaz para calibrar


					soundsDic.at(\pieI).set(\pos, (pieP.x.round).linlin(-1250, 1250, 0, 1),
						\amp, pieVel.rho.linlin(30, 200, 0, 0.75),
						\rate, pieP.y.linlin(-1350, 300, 1, 50),
						\height, pieP.y.linlin(-1350, 300, 20, 1000));
					},

					{
						soundsDic.at(\pieI).set(\pos, (pieP.x.round).linlin(-1250, 1250, 0, 1), \amp, 0.0);
				});

			},
			{
				soundsDic.add(\pieI -> Synth(\scrachIt2, [\amp, 0.0, \pos, 0.25]));
		});
	}



	//---------------------------------------
	//---------  PieD  ------------------
	//---------------------------------------

	pieD{|sk|
		var soundsDic, pie, pieVel;
		soundsDic = sk.sounds;

		if( soundsDic.at(\pieD) != nil,
			{
				pie = Cartesian(sk.pieD.pos[0], sk.pieD.pos[1], sk.pieD.pos[2]);
				pieVel = Cartesian(sk.pieD.vel[0], sk.pieD.vel[1], sk.pieD.vel[2]);
				pieVel.rho.postln;
				if(pieVel.rho > 10, {
					soundsDic.at(\pieD).set(\pos, (pie.x.round).linlin(-1250, 1250, 0, 1),
						\amp, pieVel.rho.linlin(30, 200, 0, 0.75),
						\rate, pie.y.linlin(-1350, 300, 1, 50),
						\height, pie.y.linlin(-1350, 300, 20, 1000));},
					{soundsDic.at(\pieD).set(\pos, (pie.x.round).linlin(-1250, 1250, 0, 1), \amp, 0.0);})

			},
			{
				soundsDic.add(\pieD -> Synth(\scrachIt2,[ \amp, 0.5, \pos, 0.0]));
		});
	}


	//---------------------------------------
	//---------  PieIFm  ------------------
	//---------------------------------------

	pieIFm{|sk|
		var soundsDic, pieP, pieVel, tor, dtp, fmFreq, dura, piesDist;
		soundsDic = sk.sounds;

		if( soundsDic.at(\fmResI) != nil,
			{
				fmFreq = Array.fill(10, {(betarand(70, 0.125, 0.25).asInt).midicps});
				//fmFreq = Pbrown()asStream.next;
				tor = Cartesian(sk.torso.pos[0], sk.torso.pos[1],sk.torso.pos[2]);
				pieP = Cartesian(sk.pieI.pos[0], sk.pieI.pos[1],sk.pieI.pos[2]);
				piesDist = pieP-Cartesian(sk.pieD.pos[0], sk.pieD.pos[1],sk.pieD.pos[2]);


				/*"pie.y :".post;
				pieP.y.postln;
				"pie.x :".post;
				pieP.x.postln;

				"dist :".post;
				piesDist.rho.postln;*/

				dtp = tor.dist(pieP);


				//"pos :".post;
				//pieP.y.linlin(-1350, -900, 24, 57).postln;

				pieVel = Cartesian(sk.pieI.vel[0], sk.pieI.vel[1],sk.pieI.vel[2]);
				//"pieVelMag :".post;
				//pieVel.rho.postln;


				soundsDic.at(\fmResI).set(\rate, pieVel.rho.linlin(0, 1200, 3, 300),
					\lfo1, pieP.y.linlin(-650, 400, 80, 1800),
					\pos1, pieP.x.linlin(0, 1000, 0, 1),
					\indx1, piesDist.rho.linlin( 60, 900, 5, 800),
					\indx2, piesDist.rho.linlin( 60, 900, 5, 800),
					\amp, pieVel.rho.linlin(0, 1200, 0.125, 1));
			},
			{
				soundsDic.add(\fmResI -> Synth(\fmRes, [\amp, 0.5, \note, 48]));
		});
	}



	//---------------------------------------
	//---------  PieD  ------------------
	//---------------------------------------

	pieDFm{|sk|
		var soundsDic, pieP, pieVel, tor, dtp, fmFreq, dura, piesDist;
		soundsDic = sk.sounds;

		if( soundsDic.at(\fmResD) != nil,
			{
				fmFreq = Array.fill(10, {(betarand(70, 0.125, 0.25).asInt).midicps});
				//fmFreq = Pbrown()asStream.next;
				tor = Cartesian(sk.torso.pos[0], sk.torso.pos[1],sk.torso.pos[2]);
				pieP = Cartesian(sk.pieD.pos[0], sk.pieD.pos[1],sk.pieD.pos[2]);
				piesDist = pieP-Cartesian(sk.pieI.pos[0], sk.pieI.pos[1],sk.pieI.pos[2]);


				/*"pie.y :".post;
				pieP.y.postln;
				"pie.x :".post;
				pieP.x.postln;

				"dist :".post;
				piesDist.rho.postln;*/

				dtp = tor.dist(pieP);


				//"pos :".post;
				//pieP.y.linlin(-1350, -900, 24, 57).postln;

				pieVel = Cartesian(sk.pieD.vel[0], sk.pieD.vel[1],sk.pieD.vel[2]);
				//"pieVelMag :".post;
				//pieVel.rho.postln;


				soundsDic.at(\fmResD).set(\rate, pieVel.rho.linlin(0, 1200, 3, 300),
					\lfo1, pieP.y.linlin(-650, 400, 80, 1800),
					\pos1, pieP.x.linlin(0, 1000, 0, 1),
					\indx1, piesDist.rho.linlin( 60, 900, 5, 800),
					\indx2, piesDist.rho.linlin( 60, 900, 5, 800),
					\amp, pieVel.rho.linlin(0, 1200, 0.125, 1));
			},
			{
				soundsDic.add(\fmResD -> Synth(\fmRes, [\amp, 0.5, \note, 52]));
		});
	}
	//--------------------------------
	//---------   chePlayerD   -------
	//--------------------------------

	/*makeRangeCtr{|loIn = 0, hiIn = 1, loOut = 0, hiOut = 1|
	var win, a;

	win = Window("setDeviation", Rect(300, 300, 300, 150));
	a = RangeSlider(win, Rect(10, 10, 200, 30))
	.lo_(0)
	.hi_(1);
	// w = Window("Esqueletofono", Rect(30,30, 600, 400));
	// but1 = Button(w, Rect(50, 50, 60, 40))
	// .states_([["Mano I", Color.black, Color.green],["Mano I", Color.black, Color.red]])
	// .action_({|bt|
	// 	if(bt.value == 1, {
	//
	// 	})
	// }.defer);
	//
	win.front;

	}*/

	calibrate{|ene|
		var gui, win, inRange, outRange;
		var inSlider, outSlider, txInMin, txInMax, txOutMin, txOutMax;

		gui = GUI.current;

		inRange = [0, 300];
		outRange = [0.001, 36];

		inSlider = Array.new;
		outSlider = Array.new;
		txInMin = Array.new;
		txInMax = Array.new;
		txOutMin = Array.new;
		txOutMax = Array.new;

		win = gui.window.new("Set Tunning Parameters", Rect(300, 300, 700, 300));
		ene.do({|i|

			inSlider[i] = gui.rangeSlider.new(win, Rect(70, 10+(i*30), 200, 30)).lo_(inMin[i]).hi_(inMax[i])
			.action_({|slider|
				inMin[i] = slider.lo.linlin(0.0, 1.0, inRange[0], inRange[1]);
				inMax[i] = slider.hi.linlin(0.0, 1.0, inRange[0], inRange[1]);
				[inMin[i], inMax[i]].postln;
				txInMin.string_(inMin[i].asString);
				txInMax.string_(inMax[i].asString);
			});

			outSlider[i] = gui.rangeSlider.new(win, Rect(400, 10+(i*30), 200, 30)).lo_(outMin[i]).hi_(outMax[i])
			.action_({|slider|
				outMin[i] = slider.lo.linlin(0.0, 1.0, outRange[0], outRange[1]);
				outMax[i] = slider.hi.linlin(0.0, 1.0, outRange[0], outRange[1]);
				[outMin[i], outMax[i]].postln;
				txOutMin.string_(outMin[i].asString);
				txOutMax.string_(outMax[i].asString);
			});

			txInMin[i] = gui.staticText.new(win, Rect(10, 10+(i*30), 50, 30)).background_(Color.white);
			txInMin[i].string_(inMin[i].asString);

			txInMax[i] = gui.staticText.new(win, Rect(280, 10+(i*30), 50, 30)).background_(Color.white);
			txInMax[i].string_(inMax[i].asString);

			txOutMin[i] = gui.staticText.new(win, Rect(340, 10+(i*30), 50, 30)).background_(Color.white);
			txOutMin[i].string_(outMin[i].asString);

			txOutMax[i] = gui.staticText.new(win, Rect(610, 10+(i*30), 50, 30)).background_(Color.white);
			txOutMax[i].string_(outMax[i].asString);
		});

		win.front;


	}



}