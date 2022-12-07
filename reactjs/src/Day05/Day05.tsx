import * as React from 'react';
import { useState, useEffect, useRef } from 'react';
import autoAnimate from '@formkit/auto-animate';
import { clsx } from 'clsx';

import { inputDay05 } from '../data';
import { Crate, Stack, Move, parseData } from './Parse';
import './style.scss';

export const Day05 = () => {
  const data = parseData(inputDay05);
  return <Stacks {...data} />;
};

type Props = {
  stacks: Stack[];
  moves: Move[];
};

export const Stacks = (props: Props) => {
  const [stacks, setStacks] = useState(props.stacks);
  const [moves, setMoves] = useState(props.moves);
  const parentRef = useRef(null);

  useEffect(() => {
    if (parentRef.current) {
      autoAnimate(parentRef.current);
    }
  }, [parent]);

  useEffect(() => {
    const interval = setInterval(() => {
      setMoves((moves) => {
        const move = moves.shift();

        setTimeout(() => {
          setStacks((stacks) => {
            const crate: Crate = stacks[move.from].pop();
            stacks[move.to].push(crate);
            return [...stacks];
          });
        }, 1);

        return [...moves];
      });
    }, 200);
    return () => clearInterval(interval);
  }, []);

  const currentMove = moves[0];
  // console.log({ currentMove, nextMove: moves[1] });

  return (
    <div className="app">
      <h1>Day5</h1>
      <ol className="stacks">
        {stacks.map((crates, i) => (
          <li
            key={i}
            ref={parentRef}
            className={clsx('stack', {
              from: currentMove?.from === i,
              to: currentMove?.to === i,
            })}
          >
            {crates.map((crate, j) => (
              <span
                key={j}
                className={clsx('crate', {
                  from: currentMove?.from === i && crates.length - 1 === j,
                })}
              >
                {crate}
              </span>
            ))}
            <span
              className={clsx('crate', 'placeholder', {
                to: currentMove?.to === i,
              })}
            >
              X
            </span>
          </li>
        ))}
      </ol>
      <div className="console">
        {currentMove ? (
          <span>
            {currentMove.from + 1}s to {currentMove.to + 1}
          </span>
        ) : (
          <span></span>
        )}
      </div>
    </div>
  );
};
