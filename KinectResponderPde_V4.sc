//----------------------------------
ReceiveSkeletons{

	var <>portIn, newUser, lostUser, exitUser, reEnterUser, <users, <>nUsers;

	*new{arg portR = NetAddr.langPort;
		^super.new.init(portR);
	}

	init{arg portR = NetAddr.langPort;
		nUsers = 2;
		portIn = portR;
		users = Dictionary.new;

		newUser = OSCFunc.new( {|msg, time, addr, recvPort|
			var inId;
			//make sure it comes from the correct user
			msg.postln;
			inId = msg[1];
			users.add(inId->Skeleton(inId, nUsers));

		}, '/newUser', recvPort: NetAddr.langPort);


		lostUser = OSCFunc.new( {|msg, time, addr, recvPort|
			var inId, sk;
			//make sure it comes from the correct user
			//msg.postln;
			inId = msg[1];
			sk = this.users.at(inId);
			sk.in_(false);
			sk.sounds.do({|sound|
				sound.set(\gate, 0);
			});
			sk.sounds.removeAll;
			users.removeAt(inId);

		}, '/lostUser', recvPort: NetAddr.langPort);


		exitUser = OSCFunc.new( {|msg, time, addr, recvPort|
			var inId, sk;
			//make sure it comes from the correct user
			//msg.postln;
			inId = msg[1];
			sk = this.users.at(inId);
			sk.in_(false);


		}, '/exitUser', recvPort: NetAddr.langPort);


		reEnterUser = OSCFunc.new( {|msg, time, addr, recvPort|
			var inId, sk;
			//make sure it comes from the correct user
			//msg.postln;
			inId = msg[1];
			sk = this.users.at(inId);
			sk.in_(true);


		}, '/reEnterUser', recvPort: NetAddr.langPort);

	}

}

//----------------------------------
Skeleton{

	var <usrId, <>nUsers, <>responder, <>joints, <>in, <>sounds;

	var <cabeza, <hombroD, <hombroI, <codoD, <codoI, <manoD, <manoI;
	var <torso, <caderaD, <caderaI, <rodillaD, <rodillaI, <pieD, <pieI;

	*new{arg id = 0;
		^super.new.init(id);

	}

	init{arg id, nU;

		nUsers = nU;

		in = true;

		sounds = Dictionary.new;
		joints = Dictionary.new;
		usrId = id;

		cabeza = Joint("cabeza", usrId);
		joints.add("cabeza"->cabeza);

		hombroD = Joint("hombroD", usrId);
		joints.add("hombroD"->hombroD);

		hombroI = Joint("hombroI", usrId);
		joints.add("hombroI"->hombroI);

		codoD = Joint("codoD", usrId);
		joints.add("codoD"->codoD);

		codoI = Joint("codoI", usrId);
		joints.add("codoI"->codoI);

		manoD = Joint("manoD", usrId);
		joints.add("manoD"->manoD);

		manoI = Joint("manoI", usrId);
		joints.add("manoI"->manoI);

		torso = Joint("torso", usrId);
		joints.add("torso"->torso);

		caderaD = Joint("caderaD", usrId);
		joints.add("caderaD"->caderaD);

		caderaI = Joint("caderaI", usrId);
		joints.add("caderaI"->caderaI);

		rodillaD = Joint("rodillaD", usrId);
		joints.add("rodillaD"->rodillaD);

		rodillaI = Joint("rodillaI", usrId);
		joints.add("rodillaI"->rodillaI);

		pieD = Joint("pieD", usrId);
		joints.add("pieD"->pieD);

		pieI = Joint("pieI", usrId);
		joints.add("pieI"->pieI);

		responder = OSCFunc.new( {|msg, time, addr, recvPort|
			var inId;
			//make sure it comes from the correct user
			//"mensajes a esqueleto".postln;
			//msg.postln;
			//"received ID:".postln;
			inId = msg[1];
			//"skeleton's ID".postln;
			//usrId.postln;

			if(inId == usrId,

				{
					//joints.postln;
					//msg[2].postln;
					//joints.at(msg[2].asString).postln;
					joints.at(msg[2].asString).update([msg[3], msg[4], msg[5]]);
			});


		}, '\esqueleto', recvPort: NetAddr.langPort);

	}

}

//----------------------------------
Joint{

	var <type, <usrId, <pos, <ppos, <pppos, <vel, <pvel, <acc;

	*new{arg tp = "unset", id = 0;
		^super.new.init(tp, id);

	}

	init{ arg inType, id;
		if( inType.isString == true,
			{type = inType}, {type = "unset"; "The type value should be a String"});

		usrId = id;
		pos = [0, 0, 0];
		ppos = [0, 0, 0];
		pppos = [0, 0, 0];
		vel = [0, 0, 0];
		pvel = [0, 0, 0];
		acc = [0, 0, 0];

		//Generate a OSCFunc for the corresponding Joint type and Id
		/*responder = OSCFunc.new( {|msg, time, addr, recvPort|
			var inId;
			//make sure it comes from the correct user
			//msg.postln;
			inId = msg[1];
			if(inId == usrId,
				{this.update([msg[2], msg[3], msg[4]])});

		}, type.asSymbol, recvPort: NetAddr.langPort);*/
	}


	update{arg actualpos;

		if(actualpos.isArray == true, {
			ppos = pos;
			pppos = ppos;
			pos = actualpos;
			pvel = vel;
			vel = pos - ppos;
			acc = vel - pvel;
			},
			{"the input should be an array of 3 elements"}
		);
	}


}