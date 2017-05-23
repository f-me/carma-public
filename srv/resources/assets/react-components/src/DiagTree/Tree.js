import Immutable from 'immutable'
import React from 'react'
import ReactCSSTransitionGroup from 'react-addons-css-transition-group'

import { OverlayTrigger, Tooltip } from 'react-bootstrap'
import { Glyphicon } from 'react-bootstrap'



export default class Tree extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      hoverId: null,
      expandedItems: Immutable.Map(),
      searchText: ''
    }
  }

  _onClick = it => {
    const {expandedItems, searchText} = this.state;

    if (searchText.trim() === '') {
      const isExpanded = expandedItems.get(it.id);
      this.setState({
        expandedItems: expandedItems.set(it.id, !isExpanded)
      })
    }

    this.props.onSelect(it)
  }

  _onDeleteItem = id => this.props.onDelete(id)


  _onSearch = e => {
    const searchText = e.target.value;
    const {items, selected} = this.props;
    let {expandedItems} = this.state;

    if (searchText === '') { // expand all ancestors of the selected node
      const ancestor = items.valueSeq().reduce(
        (m, x) => {
          x.answers.forEach(y => {m[y.nextSlide] = x.id});
          return m;
        }, {});
      let id = ancestor[selected];
      while (id !== undefined) {
        console.log(id);
        expandedItems = expandedItems.set(id, true);
        id = ancestor[id];
      }
    }

    this.setState({searchText, expandedItems});
  }


  _renderItem = (it, depth, ans) => {
    const {selected} = this.props;
    const {hoverId, expandedItems, searchText} = this.state;

    const itemStyle = {
      cursor: 'pointer',
      transition: 'all 0.25s ease-in-out',
    };

    const needle = searchText.trim();
    const isSearchMode = needle !== '';
    const searchRes = isSearchMode ? this._highlight(it.header, needle)  : null;

    return (
      <div className="item" key={it.id}>
        <div
          onClick={() => this._onClick(it)}
          onMouseEnter={() => this.setState({hoverId: it.id})}
          onMouseLeave={() => this.setState({hoverId: null})}
          style={{...itemStyle,
            display: (!isSearchMode || it.isRoot || searchRes !== null) ? undefined : 'none',
            paddingLeft: '4px',
            marginLeft: (depth * 16) + 'px',
            borderLeft: '5px solid '+ (it.id === selected ? '#2B95fD' : '#fff')
          }}
        >
          <div>
            <div style={{color: 'grey'}}>{ans}</div>
            {searchRes || it.header}
          </div>
          { hoverId === it.id && depth === 0
            ?  <OverlayTrigger
                  placement="top"
                  overlay={<Tooltip id="">Удалить</Tooltip>}>
                <Glyphicon
                    className="btn floating-btn"
                    onClick={() => this._onDeleteItem(it.id)}
                    glyph="trash"/>
              </OverlayTrigger>
            : ''
          }
        </div>
        {isSearchMode || expandedItems.get(it.id)
          ? this._renderChildren(it, depth+1) : ''
        }
      </div>
    );
  }

  _highlight(text, search) {
    const ix = text.search(new RegExp(search, "i"));
    if (ix === -1) return null;
    return (
      <div>
        {text.substr(0, ix)}
        <span style={{backgroundColor: "yellow"}}>
          {text.substr(ix, search.length)}
        </span>
        {text.substr(ix + search.length)}
      </div>)
  }


  _renderChildren = (it, depth) => {
    const {items} = this.props;

    return it.answers.map((ans, i) => {
      const nxt = items.get(String(ans.nextSlide));
      return nxt ? this._renderItem(nxt, depth, ans.header) : <div key={i}/>;
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

    const roots = items.valueSeq().filter(x => x.isRoot && x.isActive).toArray();

    return (
      <ReactCSSTransitionGroup transitionName="tree-list" transitionEnterTimeout={300} transitionLeaveTimeout={150}>
        <div className="Tree">
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
