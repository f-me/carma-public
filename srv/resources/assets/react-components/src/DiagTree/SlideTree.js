import React from 'react'

import { ListGroup, ListGroupItem } from 'react-bootstrap'
import { Button, ButtonToolbar, Glyphicon } from 'react-bootstrap'


export default function(props) {
  const {selectedId, slides} = props;
  const roots = slides.valueSeq().filter(x => x.isRoot).toArray();

  return (
    <div>
      <TreeNode {...props} level={0} roots={roots} />
      <ButtonToolbar>
        <Button bsStyle='success' onClick={props.onAddSlide}>
          <Glyphicon glyph="plus" /> Новое дерево
        </Button>
      </ButtonToolbar>
    </div>
  );
}


const TreeNode = (props) => {
  const {selectedId, slides, roots, level} = props;

  return (
    <ListGroup>
      {roots.map((slide, i) => {
        if (!slide) return null;

        const indent = Array(level*4).fill('\u00a0').join('');
        const selected = slide.id === selectedId;
        return (
          <div>
            <ListGroupItem key={i}
              className={selected ? 'selected' : ''}
              header={indent + slide.header}
              onClick={selected ? null : () => props.onSelect(slide.id)}
            />
            <TreeNode {...props}
              level={level+1}
              roots={slide.answers.map(ans => slides.get(String(ans.nextSlide)))}
            />
          </div>
        );
      })}
    </ListGroup>
  );
}
