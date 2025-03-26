# dungeon

Dungeon is a programming language and game engine built to define and run text-based dungeon crawler games. The language allows for definition of the following:
  - entities (these are the player and enemies)
  - for non-player entities, their behaviour is defined as a list of conditions, where each condition is associated with an action
  - a set of stats that each entity has
  - a condition to determine if an entity is alive
  - actions an entity can take
  - triggers which occur as a result of an action
  - items (these may also contain actions and triggers)
  - rooms, which may contain entities, items, and doors to other rooms

This is an example of a game:

```
game test;

statblock {
    hp = 10;
    max_hp = 10;
    str = 1;
    def = 1;
}

player me {
    stats {
        hp = 100;
        max_hp = 100;
        str = 5;
    }
    alive = hp > 0;
    action unarmed_attack {
        let dmg = 1d4 + source.str;
        target.hp = target.hp - dmg;
    }
    trigger defend on (attack) {
        source.hp = source.hp + source.def;
        if (source.hp > source.max_hp) {
            source.hp = source.max_hp;
        }
    }
    items {}
}

enemy goblin() {
    stats {}
    alive = hp > 0;
    action attack {
        let dmg = 1d4 + source.str;
        target.hp = target.hp - dmg;
    }

    action heal {
        source.hp = source.hp + 3;
        if (source.hp > source.max_hp) {
            source.hp = source.max_hp;
        }
    }

    behaviour {
        hp < 5 = heal self;
        default = attack player;
    }

    items {}
}

item key<key>() {}

room start {
    enemies {
        bob = goblin() with items {
            red_key = key<red_key>();
        };
        ted = goblin() with items {};
        steve = goblin() with items {};
    }
    items {}
    doors {
        exit to end requires (red_key);
    }
}

room end {
    enemies {}
    items {}
    doors {}
}
```
