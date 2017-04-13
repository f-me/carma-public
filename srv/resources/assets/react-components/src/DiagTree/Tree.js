
import Immutable from 'immutable'
import React from 'react'
import ReactCSSTransitionGroup from 'react-addons-css-transition-group'



export default class Tree extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      expandedItems: Immutable.Map()
    }
  }

  _onClick = it => {
    const {expandedItems} = this.state;
    this.setState({
      expandedItems: expandedItems.set(it.id, !expandedItems.get(it.id))
    })
    this.props.onSelect(it)
  }

  _renderItem = (it, depth, ans) => {
    const {selected} = this.props;
    const {expandedItems} = this.state;

    const itemStyle = {
      cursor: 'pointer',
      transition: 'all 0.25s ease-in-out',
    };

    return (
      <div key={it.id}>
        <div
          onClick={() => this._onClick(it)}
          style={{...itemStyle,
            paddingLeft: '4px',
            marginLeft: (depth * 16) + 'px',
            borderLeft: '5px solid '+ (it.id === selected ? '#2B95fD' : '#fff')
          }}
        >
          <div style={{color: 'grey'}}>{ans}</div>
          <div>{it.header}</div>
        </div>
        {!expandedItems.get(it.id) ? '' : this._renderChildren(it, depth+1)}
      </div>
    );
  }

  _renderChildren = (it, depth) => {
    const {items} = this.props;
    const {expandedItems} = this.state;

    return it.answers.map(ans => {
      const nxt = items.get(String(ans.nextSlide));
      return this._renderItem(nxt, depth, ans.header)
    });
  }

  render() {
    const {items, selected} = this.props;
    const {expandedItems} = this.state;

    const roots = items.valueSeq().filter(x => x.isRoot).toArray();

    return (
      <ReactCSSTransitionGroup transitionName="tree-list" transitionEnterTimeout={300} transitionLeaveTimeout={150}>
        <div>
          {this.props.children}
          {roots.map(it => this._renderItem(it, 0))}
        </div>
      </ReactCSSTransitionGroup>
    )
  }
}
