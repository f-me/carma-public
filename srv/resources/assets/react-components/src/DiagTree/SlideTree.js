import React from 'react'

import { ListGroup, ListGroupItem } from 'react-bootstrap'
import { Button, ButtonToolbar } from 'react-bootstrap'


export default function(props) {
  const {currentSlide, slides} = props;
  const currentId = currentSlide && currentSlide.get('id');
  const roots = slides.filter(x => x.get('isRoot'));

  return (
    <div>
      <ListGroup>
        {roots.toArray().map((slide, i) => {
          const selected = slide.get('id') === currentId;
          return (
            <ListGroupItem key={i}
              className={selected ? 'selected' : ''}
              header={slide.get('header')}
              onClick={selected ? null : () => props.onSelect(slide)}
            />
          );
        })}
      </ListGroup>
      <ButtonToolbar>
        <Button bsStyle='success'
          onClick={props.onAddSlide}
        >
          + Новое дерево
        </Button>
      </ButtonToolbar>
    </div>
  );
}
