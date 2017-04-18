
import Immutable from 'immutable'
import React from 'react'
import ReactCSSTransitionGroup from 'react-addons-css-transition-group'



export default class Tree extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      expandedItems: Immutable.Map(),
      searchText: ''
    }
  }

  _onClick = it => {
    const {expandedItems, searchText} = this.state;

    let isExpanded = expandedItems.get(it.id);
    isExpanded = !isExpanded || searchText.trim() !== '';
    this.setState({
      expandedItems: expandedItems.set(it.id, isExpanded)
    })
    this.props.onSelect(it)
  }


  _onSearch = e => this.setState({searchText: e.target.value})


  _renderItem = (it, depth, ans) => {
    const {selected} = this.props;
    const {expandedItems, searchText} = this.state;

    const itemStyle = {
      cursor: 'pointer',
      transition: 'all 0.25s ease-in-out',
    };

    const needle = searchText.trim();
    const isVisible = depth === 0
      || needle === ''
      || it.header.search(needle) >= 0;
    const isExpanded = needle !== '' || expandedItems.get(it.id);

    return (
      <div key={it.id}>
        <div
          onClick={() => this._onClick(it)}
          style={{...itemStyle,
            display: isVisible ? undefined : 'none',
            paddingLeft: '4px',
            marginLeft: (depth * 16) + 'px',
            borderLeft: '5px solid '+ (it.id === selected ? '#2B95fD' : '#fff')
          }}
        >
          <div style={{color: 'grey'}}>{ans}</div>
          <div>{it.header}</div>
        </div>
        {isExpanded ? this._renderChildren(it, depth+1) : ''}
      </div>
    );
  }

  _renderChildren = (it, depth) => {
    const {items} = this.props;

    return it.answers.map(ans => {
      const nxt = items.get(String(ans.nextSlide));
      return this._renderItem(nxt, depth, ans.header)
    });
  }

  render() {
    const {items} = this.props;
    const {searchText} = this.state;
    const searchInputStyle = {
      width: '100%',
      margin: '10px',
      padding: '4px 10px',
      marginBottom: 10,
      border: 0,
      borderBottom: '1px solid #CCCCCC'
    };

    const roots = items.valueSeq().filter(x => x.isRoot).toArray();

    return (
      <ReactCSSTransitionGroup transitionName="tree-list" transitionEnterTimeout={300} transitionLeaveTimeout={150}>
        <div>
          <input
            style={searchInputStyle}
            value={searchText}
            onChange={this._onSearch}
            type="text"
            placeholder="Поиск"
          />
          {roots.map(it => this._renderItem(it, 0))}
        </div>
      </ReactCSSTransitionGroup>
    )
  }
}
